#![crate_name = "rrworker"]

extern crate zmq;

use std::{thread, time};

fn main() {
    let context = zmq::Context::new();

    let responder = context.socket(zmq::REP).unwrap();
    assert!(responder.connect("tcp://localhost:5560").is_ok());

    loop {
        let string = responder.recv_string(0).unwrap().unwrap();
        println!("Received request: {}", string);

        thread::sleep(time::Duration::from_secs(1));

        responder.send_str(&"World", 0).unwrap();
    }
}
