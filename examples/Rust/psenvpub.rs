#![crate_name = "psenvpub"]

extern crate zmq;

use std::{thread, time};

fn main() {
    let context = zmq::Context::new();
    let publisher = context.socket(zmq::PUB).unwrap();
    assert!(publisher.bind("tcp://*:5563").is_ok());

    loop {
        publisher.send_multipart(&[b"A"], zmq::SNDMORE).unwrap();
        publisher.send_str("We don't want to see this", 0).unwrap();
        publisher.send_multipart(&[b"B"], zmq::SNDMORE).unwrap();
        publisher.send_str("We would like to see this", 0).unwrap();
        thread::sleep(time::Duration::from_secs(1));
    }
}
