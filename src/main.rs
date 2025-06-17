use std::{
    env,
    io::{IoSlice, IoSliceMut},
    mem::MaybeUninit,
    os::fd::{AsFd, OwnedFd},
    path::Path,
    process::Command,
};

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
    let client_socket_addr = SocketAddrUnix::new(runtime_dir.join(&socket_name)).expect("failed to bind client addr");
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
            transfer(&mut client, &mut server);
        }
        if server_to_client {
            transfer(&mut server, &mut client);
        }
    }
}

fn transfer(from: &mut OwnedFd, to: &mut OwnedFd) {
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

    while bytes.len() > 0 {
        match sendmsg(
            to.as_fd(),
            &[IoSlice::new(&bytes)],
            &mut send_cmsg,
            SendFlags::empty(),
        ) {
            Ok(0) => {
                std::process::exit(0);
            }
            Ok(sent) => {
                bytes = &bytes[sent..];
                send_cmsg.clear();
            }
            Err(e) => panic!("sendmsg error: {e}"),
        }
    }
}
