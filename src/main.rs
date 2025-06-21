mod protocol;

use std::{
    cmp::Ordering,
    env,
    io::{IoSlice, IoSliceMut},
    mem::MaybeUninit,
    os::{
        fd::{AsFd, BorrowedFd, OwnedFd},
        unix::net::{UnixListener, UnixStream},
    },
    path::Path,
    process::Command,
};

use protocol::{INTERFACE_TO_PROTOCOL, Interface, Message, PROTOCOLS, Type};
use rustix::{
    event::{PollFd, PollFlags, Timespec, poll},
    io::retry_on_intr,
    net::{
        RecvAncillaryBuffer, RecvAncillaryMessage, RecvFlags, SendAncillaryBuffer,
        SendAncillaryMessage, SendFlags, recvmsg, sendmsg,
    },
};
use tinywasm::{
    Extern, FuncContext, Imports, MemoryStringExt, Module, Store,
    types::{FuncType, ValType, WasmValue},
};

const CLIENT_TIMEOUT: Timespec = Timespec {
    tv_sec: 30_000,
    tv_nsec: 0,
};

fn main() {
    let wayland_display = env::var("WAYLAND_DISPLAY").expect("`WAYLAND_DISPLAY` should be set");
    let runtime_dir = env::var("XDG_RUNTIME_DIR").expect("`XDG_RUNTIME_DIR` should be set");
    let runtime_dir = Path::new(&runtime_dir);

    let socket_name = format!("waytoy-{}", std::process::id());
    let server_socket_path = runtime_dir.join(&socket_name);
    let server_socket =
        UnixListener::bind(&server_socket_path).expect("failed to bind unix socket");

    let mut args = env::args().skip(1);
    let program = args.next().expect("missing program");
    let _ = args.find(|arg| arg == "--");

    let mut store = Store::new();
    let mut module = Module::parse_file(program).expect("failed to parse wasm module");
    let mut imports = Imports::new();
    for (_, protocol) in PROTOCOLS.iter() {
        for interface in protocol.interfaces.iter() {
            for message in interface.requests.iter().chain(interface.events.iter()) {
                let arg_types = message
                    .args
                    .iter()
                    .flat_map(|arg| match arg.typ {
                        Type::Int => [ValType::I32].as_slice(),
                        Type::Uint => [ValType::I64].as_slice(),
                        Type::Fixed => [ValType::I32].as_slice(),
                        Type::String => [ValType::I32, ValType::I32].as_slice(),
                        Type::Object => [ValType::I32].as_slice(),
                        Type::NewId => [ValType::I32].as_slice(),
                        Type::Array => [ValType::I32, ValType::I32].as_slice(),
                        Type::Fd => [ValType::I32].as_slice(),
                    })
                    .copied()
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                let name = format!("{}_{}", interface.name, message.name);
                imports
                    .define(
                        "wayland",
                        &name,
                        Extern::func(
                            &FuncType {
                                params: arg_types,
                                results: Box::new([]),
                            },
                            move |mut ctx: FuncContext<'_>, args| {
                                #[derive(Debug)]
                                pub enum Arg {
                                    Int(i32),
                                    Uint(u32),
                                    Fixed(f64),
                                    String(String),
                                    Object(u32),
                                    NewId(u32),
                                    Array(Vec<u8>),
                                    Fd(i32),
                                }
                                let mut args = args.into_iter();
                                let args = message
                                    .args
                                    .iter()
                                    .map(|arg| match arg.typ {
                                        Type::Int => {
                                            let WasmValue::I32(int) = args.next().unwrap() else {
                                                unreachable!()
                                            };
                                            Arg::Int(*int)
                                        }
                                        Type::Uint => {
                                            let WasmValue::I64(uint) = args.next().unwrap() else {
                                                unreachable!()
                                            };
                                            Arg::Uint(*uint as u32)
                                        }
                                        Type::Fixed => {
                                            let WasmValue::I32(fixed) = args.next().unwrap() else {
                                                unreachable!()
                                            };
                                            Arg::Fixed(*fixed as f64 / 256.0)
                                        }
                                        Type::String => {
                                            let WasmValue::I32(len) = args.next().unwrap() else {
                                                unreachable!()
                                            };
                                            let WasmValue::I32(ptr) = args.next().unwrap() else {
                                                unreachable!()
                                            };
                                            let mem = ctx.exported_memory("memory").unwrap();
                                            let string = mem
                                                .load_string(*ptr as usize, *len as usize)
                                                .unwrap();
                                            Arg::String(string)
                                        }
                                        Type::Object => {
                                            let WasmValue::I32(object) = args.next().unwrap()
                                            else {
                                                unreachable!()
                                            };
                                            Arg::Object(*object as u32)
                                        }
                                        Type::NewId => {
                                            let WasmValue::I32(new_id) = args.next().unwrap()
                                            else {
                                                unreachable!()
                                            };
                                            Arg::Object(*new_id as u32)
                                        }
                                        Type::Array => {
                                            let WasmValue::I32(len) = args.next().unwrap() else {
                                                unreachable!()
                                            };
                                            let WasmValue::I32(ptr) = args.next().unwrap() else {
                                                unreachable!()
                                            };
                                            let mem = ctx.exported_memory("memory").unwrap();
                                            let bytes =
                                                mem.load_vec(*ptr as usize, *len as usize).unwrap();
                                            Arg::Array(bytes)
                                        }
                                        Type::Fd => {
                                            let WasmValue::I32(fd) = args.next().unwrap() else {
                                                unreachable!()
                                            };
                                            Arg::Int(*fd)
                                        }
                                    })
                                    .collect::<Vec<_>>();
                                dbg!(args);
                                Ok(Vec::new())
                            },
                        ),
                    )
                    .unwrap();
            }
        }
    }

    let mut instance = module
        .instantiate(&mut store, Some(imports))
        .expect("failed to instantiate wasm module");
    let mut memory = instance.exported_memory_mut(&mut store, "memory").unwrap();
    memory.store(20000, 4, b"TEST").unwrap();
    let mut callback = instance
        .exported_func::<(i64, i32, i32, i64), ()>(&store, "wl_registry_global")
        .unwrap();
    let result = callback.call(&mut store, (1, 20000, 4, 2)).unwrap();

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
    let (client, _) = match server_socket.accept() {
        Ok(client) => client,
        Err(e) => panic!("accept error: {e}"),
    };
    let socket_path = runtime_dir.join(&wayland_display);
    let server = UnixStream::connect(socket_path).expect("failed to open unix socket");

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
            transfer(
                &mut state,
                MessageKind::Request,
                client.as_fd(),
                server.as_fd(),
            );
        }
        if server_to_client {
            transfer(
                &mut state,
                MessageKind::Event,
                server.as_fd(),
                client.as_fd(),
            );
        }
    }
}

fn transfer(state: &mut State, kind: MessageKind, from: BorrowedFd, to: BorrowedFd) {
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
