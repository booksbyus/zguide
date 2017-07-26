#![crate_name = "taskwork2"]

extern crate zmq;

use std::io::{self, Write};
use std::{thread, time};

fn atoi(s: &str) -> i64 {
    s.parse().unwrap()
}

fn main() {
    let context = zmq::Context::new();
    let receiver = context.socket(zmq::PULL).unwrap();
    assert!(receiver.connect("tcp://localhost:5557").is_ok());

    let sender = context.socket(zmq::PUSH).unwrap();
    assert!(sender.connect("tcp://localhost:5558").is_ok());

    let control = context.socket(zmq::SUB).unwrap();
    assert!(control.connect("tcp://localhost:5559").is_ok());
    assert!(control.set_subscribe(b"").is_ok());

    let items = &mut [
        receiver.as_poll_item(zmq::POLLIN),
        control.as_poll_item(zmq::POLLIN),
    ];

    loop {
        zmq::poll(items, -1).unwrap();
        if items[0].get_revents() & zmq::POLLIN != 0 {
            let string = receiver.recv_string(0).unwrap().unwrap();
            println!("{}.", string);
            let _ = io::stdout().flush();
            thread::sleep(time::Duration::from_millis(atoi(&string) as u64));
            sender.send_str(&"", 0).unwrap();
        }
        if items[1].get_revents() & zmq::POLLIN != 0 {
            break;
        }
    }
}
