#![crate_name = "taskwork"]

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

    loop {
        let string = receiver.recv_string(0).unwrap().unwrap();
        println!("{}.", string);
        let _ = io::stdout().flush();
        thread::sleep(time::Duration::from_millis(atoi(&string) as u64));
        sender.send_str(&"", 0).unwrap();
    }
}
