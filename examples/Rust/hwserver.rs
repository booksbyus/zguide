#![crate_name = "hwserver"]

extern crate zmq;

use std::{thread, time};

fn main() {
    let context = zmq::Context::new();
    let responder = context.socket(zmq::REP).unwrap();
    assert!(responder.bind("tcp://*:5555").is_ok());

    loop {
        let buffer = &mut [0; 10];
        responder.recv_into(buffer, 0).unwrap();
        println!("Received Hello");
        thread::sleep(time::Duration::from_secs(1));
        responder.send_str("World", 0).unwrap();
    }
}
