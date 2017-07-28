#![crate_name = "interrupt"]

extern crate nix;
extern crate zmq;

use std::os::unix::io::RawFd;
use std::process;
use nix::sys::signal;

static mut S_FD: RawFd = -1;

extern "C" fn s_signal_handler(_: i32) {
    let rc = unsafe { nix::unistd::write(S_FD, b" ") };
    match rc {
        Ok(_) => {}
        Err(_) => {
            println!("Error while writing to self-pipe.");
            process::exit(1);
        }
    }
}

fn s_catch_signals(fd: RawFd) {
    unsafe {
        S_FD = fd;
    }

    let sig_action = signal::SigAction::new(
        signal::SigHandler::Handler(s_signal_handler),
        signal::SaFlags::empty(),
        signal::SigSet::empty(),
    );
    unsafe {
        signal::sigaction(signal::SIGINT, &sig_action).unwrap();
        signal::sigaction(signal::SIGTERM, &sig_action).unwrap();
    }
}

fn main() {
    let context = zmq::Context::new();
    let socket = context.socket(zmq::REP).unwrap();
    assert!(socket.bind("tcp://*:5555").is_ok());

    let pipefds = nix::unistd::pipe().unwrap();
    nix::fcntl::fcntl(pipefds.0, nix::fcntl::F_GETFL).unwrap();
    nix::fcntl::fcntl(pipefds.0, nix::fcntl::F_SETFL(nix::fcntl::O_NONBLOCK)).unwrap();

    s_catch_signals(pipefds.1);

    let items = &mut [
        zmq::PollItem::from_fd(pipefds.0, zmq::POLLIN),
        socket.as_poll_item(zmq::POLLIN),
    ];

    loop {
        let rc = zmq::poll(items, -1);
        match rc {
            Ok(v) => {
                assert!(v >= 0);
                if v == 0 {
                    continue;
                }
                if items[0].is_readable() {
                    let buffer = &mut [0; 1];
                    nix::unistd::read(pipefds.0, buffer).unwrap();
                    println!("W: interrupt received, killing server...");
                    break;
                }
                if items[1].is_readable() {
                    let buffer = &mut [0; 255];
                    let rc = socket.recv_into(buffer, zmq::DONTWAIT);
                    match rc {
                        Ok(_) => println!("W: recv"),
                        Err(e) => {
                            if e == zmq::Error::EAGAIN || e == zmq::Error::EINTR {
                                continue;
                            }
                            println!("recv: {}", e);
                            process::exit(1);
                        }
                    }
                }
            }
            Err(e) => {
                if e == zmq::Error::EINTR {
                    continue;
                }
                println!("zmq::poll: {}", e);
                process::exit(1);
            }
        }
    }
}
