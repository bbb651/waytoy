mod protocol;

use std::{
    cmp::Ordering,
    env,
    io::{IoSlice, IoSliceMut},
    mem::MaybeUninit,
    os::fd::{AsFd, OwnedFd},
    path::Path,
    process::Command,
};

use protocol::{INTERFACE_TO_PROTOCOL, Interface, PROTOCOLS, Type};
use rustix::{
    event::{PollFd, PollFlags, Timespec, poll},
    io::retry_on_intr,
    net::{
        AddressFamily, RecvAncillaryBuffer, RecvAncillaryMessage, RecvFlags, SendAncillaryBuffer,
        SendAncillaryMessage, SendFlags, SocketAddrUnix, SocketFlags, SocketType, accept_with,
        bind, connect, listen, recvmsg, sendmsg, socket_with,
    },
};

const CLIENT_TIMEOUT: Timespec = Timespec {
    tv_sec: 30_000,
    tv_nsec: 0,
};

fn main() {
    let wayland_display = env::var("WAYLAND_DISPLAY").expect("`WAYLAND_DISPLAY` should be set");
    let runtime_dir = env::var("XDG_RUNTIME_DIR").expect("`XDG_RUNTIME_DIR` should be set");
    let runtime_dir = Path::new(&runtime_dir);

    let socket_path = runtime_dir.join(&wayland_display);
    let server_socket_addr = SocketAddrUnix::new(&socket_path).expect("failed to bind server addr");
    let server_socket = socket_with(
        AddressFamily::UNIX,
        SocketType::STREAM,
        SocketFlags::CLOEXEC | SocketFlags::NONBLOCK,
        None,
    )
    .expect("failed to open unix socket");

    let socket_name = format!("waytoy-{}", std::process::id());
    let client_socket_addr =
        SocketAddrUnix::new(runtime_dir.join(&socket_name)).expect("failed to bind client addr");
    bind(&server_socket, &client_socket_addr).expect("failed to bind unix socket");
    listen(&server_socket, 128).expect("failed to set server socket to listen mode");

    let mut args = env::args();
    let _ = args.find(|arg| arg == "--");

    Command::new(args.next().expect("missing client executable"))
        .args(args)
        .env("WAYLAND_DISPLAY", socket_name)
        .spawn()
        .expect("failed to execute child");

    let mut poll_fds = [PollFd::from_borrowed_fd(
        server_socket.as_fd(),
        PollFlags::IN,
    )];
    match retry_on_intr(|| poll(poll_fds.as_mut(), Some(&CLIENT_TIMEOUT))) {
        Ok(_) => {}
        Err(e) => panic!("poll error: {e}"),
    }
    let mut client = match accept_with(&server_socket, SocketFlags::CLOEXEC) {
        Ok(client) => client,
        Err(e) => panic!("accept error: {e}"),
    };
    let mut server = socket_with(
        AddressFamily::UNIX,
        SocketType::STREAM,
        SocketFlags::CLOEXEC,
        None,
    )
    .expect("failed to open unix socket");
    match connect(&server, &server_socket_addr) {
        Ok(_) => {}
        Err(e) => panic!("unexpected error on connect() {}", e),
    };

    let mut state = State {
        objects: ObjectMap::new(),
    };
    loop {
        let mut poll_fds = [
            PollFd::from_borrowed_fd(server.as_fd(), PollFlags::IN),
            PollFd::from_borrowed_fd(client.as_fd(), PollFlags::IN),
        ];
        match retry_on_intr(|| poll(poll_fds.as_mut(), Some(&CLIENT_TIMEOUT))) {
            Ok(_) => {}
            Err(e) => panic!("poll error: {e}"),
        }

        let server_to_client = poll_fds[0].revents().contains(PollFlags::IN);
        let client_to_server = poll_fds[1].revents().contains(PollFlags::IN);

        poll_fds[0].clear_revents();
        poll_fds[1].clear_revents();

        if client_to_server {
            transfer(&mut state, MessageKind::Request, &mut client, &mut server);
        }
        if server_to_client {
            transfer(&mut state, MessageKind::Event, &mut server, &mut client);
        }
    }
}

fn transfer(state: &mut State, kind: MessageKind, from: &mut OwnedFd, to: &mut OwnedFd) {
    let mut bytes = [0u8; 4096];
    let mut space = [MaybeUninit::uninit(); rustix::cmsg_space!(ScmRights(253))];
    let mut fds: Vec<OwnedFd> = Vec::new();

    let mut recv_cmsg = RecvAncillaryBuffer::new(&mut space);

    let recv = match recvmsg(
        from.as_fd(),
        &mut [IoSliceMut::new(&mut bytes)],
        &mut recv_cmsg,
        RecvFlags::CMSG_CLOEXEC,
    ) {
        Ok(recv) => recv,
        Err(e) => panic!("recv error: {e}"),
    };

    if recv.bytes == 0 {
        std::process::exit(0);
    }

    let mut bytes = &bytes[0..recv.bytes];
    fds.extend(
        recv_cmsg
            .drain()
            .flat_map(|msg| match msg {
                RecvAncillaryMessage::ScmRights(rights) => Some(rights),
                _ => None,
            })
            .flatten(),
    );
    drop(recv_cmsg);

    space.fill(MaybeUninit::uninit());
    let mut send_cmsg = SendAncillaryBuffer::new(&mut space);
    let to_send: Vec<_> = fds.iter().map(|fd| fd.as_fd()).collect();
    send_cmsg.push(SendAncillaryMessage::ScmRights(to_send.as_slice()));

    while let Some(length) = parse_message(state, &bytes[..], kind) {
        let (mut to_send, rest) = bytes.split_at(length);
        while to_send.len() > 0 {
            match sendmsg(
                to.as_fd(),
                &[IoSlice::new(&to_send)],
                &mut send_cmsg,
                SendFlags::empty(),
            ) {
                Ok(0) => {
                    std::process::exit(0);
                }
                Ok(sent) => {
                    to_send = &to_send[sent..];
                    send_cmsg.clear();
                }
                Err(e) => panic!("sendmsg error: {e}"),
            }
        }
        bytes = rest;
    }
}

fn parse_message(state: &mut State, mut bytes: &[u8], kind: MessageKind) -> Option<usize> {
    if bytes.len() < 2 * 4 {
        return None;
    }
    let id = u32::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
    let (opcode, length) = {
        let word = u32::from_ne_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]);
        ((word & 0xffff) as u16, (word >> 16) as u16)
    };
    // dbg!(id, opcode);
    let message = state
        .objects
        .find(dbg!(id))
        .and_then(|o| {
            let interface = o.interface.known();
            // dbg!(opcode);
            // dbg!(interface);
            match kind {
                MessageKind::Request => interface.requests.get(opcode as usize),
                MessageKind::Event => interface.events.get(opcode as usize),
            }
        })
        .expect("invalid message");
    // dbg!(&message.name);
    bytes = &bytes[8..length as usize];
    let mut offset = 0;
    for arg in message.args.iter() {
        // dbg!(arg);
        offset += match arg.typ {
            Type::Int => 4,
            Type::Uint => 4,
            Type::Fixed => 4,
            Type::String => {
                4 + u32::from_ne_bytes([
                    bytes[offset],
                    bytes[offset + 1],
                    bytes[offset + 2],
                    bytes[offset + 3],
                ]) as usize
            }
            Type::Object => 4,
            Type::NewId => {
                let mut len = 0;
                // dbg!(offset);
                let interface = arg.interface.clone().unwrap_or_else(|| {
                    // dbg!(&bytes);
                    let name_len = u32::from_ne_bytes([
                        bytes[offset],
                        bytes[offset + 1],
                        bytes[offset + 2],
                        bytes[offset + 3],
                    ]);
                    // dbg!(name_len);
                    let _version = u32::from_ne_bytes([
                        bytes[offset + 4 + name_len as usize],
                        bytes[offset + 4 + name_len as usize + 1],
                        bytes[offset + 4 + name_len as usize + 2],
                        bytes[offset + 4 + name_len as usize + 3],
                    ]);
                    len += u32::div_ceil(name_len, 4) * 4 + 8;
                    String::from_utf8(Vec::from(
                        &bytes[offset + 4..offset + 4 + name_len as usize - 1],
                    ))
                    .expect("interface names must be valid utf-8")
                });
                let protocol = &PROTOCOLS[&INTERFACE_TO_PROTOCOL[dbg!(&interface)]];
                let object = Object {
                    interface: InterfaceRef::Known(
                        protocol
                            .interfaces
                            .iter()
                            .find(|Interface { name, .. }| name == &interface)
                            .expect("missing protocol"),
                    ),
                };
                let object_id = u32::from_ne_bytes([
                    bytes[offset + len as usize],
                    bytes[offset + len as usize + 1],
                    bytes[offset + len as usize + 2],
                    bytes[offset + len as usize + 3],
                ]);
                dbg!(object_id);
                len += 4;
                match kind {
                    MessageKind::Request => state.objects.insert_at(object_id, object),
                    MessageKind::Event => state.objects.insert_at(object_id, object),
                };
                len as usize
            }
            Type::Array => {
                4 + u32::from_ne_bytes([
                    bytes[offset],
                    bytes[offset + 1],
                    bytes[offset + 2],
                    bytes[offset + 3],
                ]) as usize
            }
            Type::Fd => 0,
        };
    }
    Some(length as usize)
}

struct State {
    objects: ObjectMap,
}

#[derive(Debug, Clone)]
pub struct Object {
    interface: InterfaceRef,
}

#[derive(Debug, Clone)]
pub enum InterfaceRef {
    Known(&'static Interface),
    Unknown(String),
}

impl InterfaceRef {
    pub fn name(&self) -> &str {
        match self {
            InterfaceRef::Known(interface) => &interface.name,
            InterfaceRef::Unknown(name) => name,
        }
    }

    pub fn known(&self) -> &'static Interface {
        match self {
            InterfaceRef::Known(interface) => interface,
            InterfaceRef::Unknown(name) => {
                panic!("expected known interface, found unknown interface `{name}`")
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum MessageKind {
    Request,
    Event,
}

const SERVER_ID_LIMIT: u32 = 0xFF00_0000;

#[derive(Debug, Default)]
pub struct ObjectMap {
    client_objects: Vec<Option<Object>>,
    server_objects: Vec<Option<Object>>,
}

impl ObjectMap {
    pub fn new() -> Self {
        ObjectMap {
            client_objects: vec![Some(Object {
                interface: InterfaceRef::Known(&PROTOCOLS["wayland"].interfaces[0]),
            })],
            server_objects: vec![],
        }
    }

    pub fn find(&self, id: u32) -> Option<Object> {
        if id == 0 {
            None
        } else if id >= SERVER_ID_LIMIT {
            self.server_objects
                .get((id - SERVER_ID_LIMIT) as usize)
                .and_then(Clone::clone)
        } else {
            self.client_objects
                .get((id - 1) as usize)
                .and_then(Clone::clone)
        }
    }

    pub fn remove(&mut self, id: u32) {
        if id == 0 {
        } else if id >= SERVER_ID_LIMIT {
            if let Some(place) = self.server_objects.get_mut((id - SERVER_ID_LIMIT) as usize) {
                *place = None;
            }
        } else if let Some(place) = self.client_objects.get_mut((id - 1) as usize) {
            *place = None;
        }
    }

    pub fn insert_at(&mut self, id: u32, object: Object) -> Result<(), ()> {
        if id == 0 {
            Err(())
        } else if id >= SERVER_ID_LIMIT {
            insert_in_at(
                &mut self.server_objects,
                (id - SERVER_ID_LIMIT) as usize,
                object,
            )
        } else {
            insert_in_at(&mut self.client_objects, (id - 1) as usize, object)
        }
    }

    pub fn client_insert_new(&mut self, object: Object) -> u32 {
        insert_in(&mut self.client_objects, object) + 1
    }

    pub fn server_insert_new(&mut self, object: Object) -> u32 {
        insert_in(&mut self.server_objects, object) + SERVER_ID_LIMIT
    }
}

fn insert_in(store: &mut Vec<Option<Object>>, object: Object) -> u32 {
    match store.iter().position(Option::is_none) {
        Some(id) => {
            store[id] = Some(object);
            id as u32
        }
        None => {
            store.push(Some(object));
            (store.len() - 1) as u32
        }
    }
}

fn insert_in_at(store: &mut Vec<Option<Object>>, id: usize, object: Object) -> Result<(), ()> {
    match id.cmp(&store.len()) {
        Ordering::Greater => Err(()),
        Ordering::Equal => {
            store.push(Some(object));
            Ok(())
        }
        Ordering::Less => {
            let previous = &mut store[id];
            if !previous.is_none() {
                return Err(());
            }
            *previous = Some(object);
            Ok(())
        }
    }
}
