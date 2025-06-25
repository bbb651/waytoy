pub mod protocol;

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
    sync::mpsc::Receiver,
};

use protocol::{INTERFACE_TO_PROTOCOL, Interface, PROTOCOLS, Type};
use rustix::{
    event::{PollFd, PollFlags, Timespec, poll},
    io::retry_on_intr,
    net::{
        RecvAncillaryBuffer, RecvAncillaryMessage, RecvFlags, SendAncillaryBuffer,
        SendAncillaryMessage, SendFlags, recvmsg, sendmsg,
    },
};
use tinywasm::{
    Extern, FuncContext, Imports, MemoryStringExt, Module, ModuleInstance, Store,
    types::{FuncType, ValType, WasmValue},
};

use crate::protocol::MessageType;

const CLIENT_TIMEOUT: Timespec = Timespec {
    tv_sec: 30_000,
    tv_nsec: 0,
};

#[derive(Debug)]
struct Message {
    object_id: u32,
    opcode: u16,
    kind: MessageKind,
    args: Vec<Arg>,
}

impl Message {
    pub fn size(&self) -> usize {
        8 + self.args.iter().map(Arg::size).sum::<usize>()
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        fn write_i32(bytes: &mut Vec<u8>, n: i32) {
            bytes.extend_from_slice(&n.to_ne_bytes()[..]);
        }
        fn write_u32(bytes: &mut Vec<u8>, n: u32) {
            bytes.extend_from_slice(&n.to_ne_bytes()[..]);
        }
        fn write_array(bytes: &mut Vec<u8>, b: &[u8]) {
            let len = b.len();
            write_u32(bytes, len as u32);
            for b in b {
                bytes.push(*b);
            }
            for _ in 0..(len.div_ceil(4) * 4) - len {
                bytes.push(0);
            }
        }
        fn write_string(bytes: &mut Vec<u8>, s: &str) {
            let len = s.len() + 1;
            write_u32(bytes, len as u32);
            for b in s.bytes() {
                bytes.push(b);
            }
            bytes.push(0);
            for _ in 0..(len.div_ceil(4) * 4) - len {
                bytes.push(0);
            }
        }

        let mut bytes = Vec::new();

        write_u32(&mut bytes, self.object_id);
        // Backpatched
        write_u32(&mut bytes, 0);

        for arg in self.args.iter() {
            match arg {
                Arg::Int(int) => write_i32(&mut bytes, *int),
                Arg::Uint(uint) => write_u32(&mut bytes, *uint),
                Arg::Fixed(fixed) => write_i32(&mut bytes, (fixed * 256.0) as i32),
                Arg::String(string) => write_string(&mut bytes, string),
                Arg::Object(object_id) => write_u32(&mut bytes, *object_id),
                Arg::NewId {
                    object_id,
                    interface,
                } => {
                    if let Some((name, version)) = interface {
                        write_string(&mut bytes, name);
                        write_u32(&mut bytes, *version);
                    }
                    write_u32(&mut bytes, *object_id);
                }
                Arg::Array(items) => write_array(&mut bytes, items),
                Arg::Fd(_) => {}
            }
        }

        let len = bytes.len();
        bytes[4..8].copy_from_slice(&(self.opcode as u32 | (len as u32) << 16).to_ne_bytes());
        bytes
    }

    pub fn to_string(&self, object_map: &ObjectMap) -> String {
        let Self {
            object_id,
            kind,
            opcode,
            ..
        } = self;
        let (object, method) = object_map
            .find(*object_id)
            .map(|o| {
                let interface = o.interface.known();
                (
                    interface.name.as_str(),
                    match kind {
                        MessageKind::Request => interface.requests.get(*opcode as usize),
                        MessageKind::Event => interface.events.get(*opcode as usize),
                    }
                    .map(|message| message.name.as_str())
                    .unwrap_or("MISSING"),
                )
            })
            .unwrap_or(("MISSING", "MISSING"));
        let args = self
            .args
            .iter()
            .map(|arg| match arg {
                Arg::Int(int) => int.to_string(),
                Arg::Uint(uint) => uint.to_string(),
                Arg::Fixed(fixed) => fixed.to_string(),
                Arg::String(string) => format!("{string:?}"),
                Arg::Object(object_id) => {
                    let object = object_map
                        .find(*object_id)
                        .map(|o| o.interface.known().name.as_str())
                        .unwrap_or("MISSING");
                    format!("{object}#{object_id}")
                }
                Arg::NewId {
                    object_id,
                    interface: Some((name, _version)),
                } => {
                    format!("new id {name}#{object_id}")
                }
                Arg::NewId {
                    object_id,
                    interface: None,
                } => {
                    let object = object_map
                        .find(*object_id)
                        .map(|o| o.interface.known().name.as_str())
                        .unwrap_or("MISSING");
                    format!("new id {object}#{object_id}")
                }
                Arg::Array(items) => {
                    format!("array[{}]", items.len())
                }
                Arg::Fd(fd) => format!("fd {fd}"),
            })
            .collect::<Vec<String>>()
            .join(", ");
        let prefix = match kind {
            MessageKind::Request => " -> ",
            MessageKind::Event => "",
        };
        format!("{prefix}{object}#{object_id}.{method}({args})")
    }
}

#[derive(Debug)]
enum Arg {
    Int(i32),
    Uint(u32),
    Fixed(f64),
    String(String),
    Object(u32),
    NewId {
        object_id: u32,
        interface: Option<(String, u32)>,
    },
    Array(Vec<u8>),
    Fd(i32),
}

impl Arg {
    pub fn size(&self) -> usize {
        match self {
            Arg::Int(_) => 4,
            Arg::Uint(_) => 4,
            Arg::Fixed(_) => 4,
            Arg::String(s) => 4 + (s.len() + 1).div_ceil(4) * 4,
            Arg::Object(_) => 4,
            Arg::NewId {
                interface: None, ..
            } => 4,
            Arg::NewId {
                interface: Some((name, _)),
                ..
            } => 12 + name.len().div_ceil(4) * 4,
            Arg::Array(items) => 4 + items.len().div_ceil(4) * 4,
            Arg::Fd(_) => 0,
        }
    }
}

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
    let module = Module::parse_file(program).expect("failed to parse wasm module");
    let mut imports = Imports::new();
    let (tx, rx) = std::sync::mpsc::sync_channel::<Vec<Arg>>(1);
    for (_, protocol) in PROTOCOLS.iter() {
        for interface in protocol.interfaces.iter() {
            for message in interface.requests.iter().chain(interface.events.iter()) {
                let arg_types = message
                    .args
                    .iter()
                    .flat_map(|arg| match arg.ty {
                        Type::Int => [ValType::I64].as_slice(),
                        Type::Uint => [ValType::I32].as_slice(),
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
                            {
                                let tx = tx.clone();
                                move |mut ctx: FuncContext<'_>, args| {
                                    let mut args = args.into_iter().rev();
                                    let args = message
                                        .args
                                        .iter()
                                        .map(|arg| match arg.ty {
                                            Type::Int => {
                                                let WasmValue::I32(int) = args.next().unwrap()
                                                else {
                                                    unreachable!()
                                                };
                                                Arg::Int(*int)
                                            }
                                            Type::Uint => {
                                                let WasmValue::I32(uint) = args.next().unwrap()
                                                else {
                                                    unreachable!()
                                                };
                                                Arg::Uint(*uint as u32)
                                            }
                                            Type::Fixed => {
                                                let WasmValue::I32(fixed) = args.next().unwrap()
                                                else {
                                                    unreachable!()
                                                };
                                                Arg::Fixed(*fixed as f64 / 256.0)
                                            }
                                            Type::String => {
                                                let WasmValue::I32(len) = args.next().unwrap()
                                                else {
                                                    unreachable!()
                                                };
                                                let WasmValue::I32(ptr) = args.next().unwrap()
                                                else {
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
                                                let WasmValue::I32(len) = args.next().unwrap()
                                                else {
                                                    unreachable!()
                                                };
                                                let WasmValue::I32(ptr) = args.next().unwrap()
                                                else {
                                                    unreachable!()
                                                };
                                                let mem = ctx.exported_memory("memory").unwrap();
                                                let bytes = mem
                                                    .load_vec(*ptr as usize, *len as usize)
                                                    .unwrap();
                                                Arg::Array(bytes)
                                            }
                                            Type::Fd => {
                                                let WasmValue::I32(fd) = args.next().unwrap()
                                                else {
                                                    unreachable!()
                                                };
                                                Arg::Int(*fd)
                                            }
                                        })
                                        .collect::<Vec<_>>();
                                    tx.send(args).unwrap();
                                    Ok(Vec::new())
                                }
                            },
                        ),
                    )
                    .unwrap();
            }
        }
    }

    let instance = module
        .instantiate(&mut store, Some(imports))
        .expect("failed to instantiate wasm module");

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
        store,
        module: instance,
        rx,
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

    while let Some(mut message) = parse_message(state, &bytes[..], kind) {
        let (mut to_send, rest) = bytes.split_at(message.size());

        if let Some(interface) = state
            .objects
            .find(message.object_id)
            .map(|o| o.interface.known())
        {
            let name = match kind {
                MessageKind::Request => &interface.requests[message.opcode as usize].name,
                MessageKind::Event => &interface.events[message.opcode as usize].name,
            };

            if let Ok(callback) = state
                .module
                .exported_func_untyped(&state.store, &format!("{}_{}", &interface.name, name))
            {
                let args = message
                    .args
                    .iter()
                    .map(|arg| match arg {
                        Arg::Uint(uint) => WasmValue::I32(*uint as i32),
                        _ => todo!(),
                    })
                    .collect::<Vec<_>>();

                let _ = callback.call(&mut state.store, &args).unwrap();
                let args = state.rx.recv().unwrap();
                message.args = args;
                to_send = &message.to_bytes().leak()[..];
            }
        }

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

fn parse_message(state: &mut State, mut bytes: &[u8], kind: MessageKind) -> Option<Message> {
    fn read_i32(bytes: &mut &[u8]) -> i32 {
        let b;
        (b, *bytes) = bytes.split_at(4);
        i32::from_ne_bytes([b[0], b[1], b[2], b[3]])
    }
    fn read_u32(bytes: &mut &[u8]) -> u32 {
        let b;
        (b, *bytes) = bytes.split_at(4);
        u32::from_ne_bytes([b[0], b[1], b[2], b[3]])
    }
    fn read_array(bytes: &mut &[u8]) -> Vec<u8> {
        let size = read_u32(bytes);
        let array;
        (array, *bytes) = bytes.split_at((size.div_ceil(4) * 4) as usize);
        array[..size as usize].to_vec()
    }
    fn read_string(bytes: &mut &[u8]) -> String {
        let mut bytes = read_array(bytes);
        assert_eq!(bytes.pop(), Some(b'\0'));
        String::from_utf8(bytes).expect("invalid utf-8 in string")
    }

    if bytes.len() < 2 * 4 {
        return None;
    }
    let object_id = read_u32(&mut bytes);
    let (opcode, length) = {
        let word = read_u32(&mut bytes);
        ((word & 0xffff) as u16, (word >> 16) as u16)
    };

    let message = state
        .objects
        .find(object_id)
        .and_then(|o| {
            let interface = o.interface.known();
            match kind {
                MessageKind::Request => interface.requests.get(opcode as usize),
                MessageKind::Event => interface.events.get(opcode as usize),
            }
        })
        .inspect(|message| {
            if let Some(MessageType::Destructor) = message.ty {
                state.objects.remove(object_id);
            }
        })
        .expect("invalid message");

    let args = message
        .args
        .iter()
        .map(|arg| match arg.ty {
            Type::Int => Arg::Int(read_i32(&mut bytes)),
            Type::Uint => Arg::Uint(read_u32(&mut bytes)),
            Type::Fixed => Arg::Fixed(read_u32(&mut bytes) as f64 / 256.0),
            Type::String => Arg::String(read_string(&mut bytes)),
            Type::Object => Arg::Object(read_u32(&mut bytes)),
            Type::NewId => {
                let (interface, interface_name) = if let Some(interface) = arg.interface.as_ref() {
                    (None, interface)
                } else {
                    let name = read_string(&mut bytes);
                    let version = read_u32(&mut bytes);
                    (Some((name.clone(), version)), &name.clone())
                };
                let protocol = &PROTOCOLS[&INTERFACE_TO_PROTOCOL[interface_name]];
                let object = Object {
                    interface: InterfaceRef::Known(
                        protocol
                            .interfaces
                            .iter()
                            .find(|Interface { name, .. }| name == interface_name)
                            .expect("missing protocol"),
                    ),
                };
                let new_id = read_u32(&mut bytes);
                let _ = match kind {
                    MessageKind::Request => state.objects.insert_at(new_id, object),
                    MessageKind::Event => state.objects.insert_at(new_id, object),
                }
                .unwrap();
                Arg::NewId {
                    object_id: new_id,
                    interface,
                }
            }
            Type::Array => Arg::Array(read_array(&mut bytes)),
            Type::Fd => Arg::Fd(0),
        })
        .collect();
    let message = Message {
        object_id,
        opcode,
        kind,
        args,
    };
    println!("{}", message.to_string(&state.objects));
    assert_eq!(message.size() as u16, length);
    Some(message)
}

struct State {
    objects: ObjectMap,
    store: Store,
    module: ModuleInstance,
    rx: Receiver<Vec<Arg>>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
            // server_objects: vec![Some(Object {
            //     interface: InterfaceRef::Known(&PROTOCOLS["wayland"].interfaces[0]),
            // })],
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
