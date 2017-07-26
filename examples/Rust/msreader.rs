#![crate_name = "msreader"]

extern crate zmq;

use std::{thread, time};

fn main() {
    let context = zmq::Context::new();
    let receiver = context.socket(zmq::PULL).unwrap();
    assert!(receiver.connect("tcp://localhost:5557").is_ok());

    let subscriber = context.socket(zmq::SUB).unwrap();
    assert!(subscriber.connect("tcp://localhost:5556").is_ok());
    assert!(subscriber.set_subscribe(b"10001").is_ok());

    loop {
        loop {
            if receiver.recv_msg(zmq::DONTWAIT).is_err() {
                break;
            }
        }
        loop {
            if subscriber.recv_msg(zmq::DONTWAIT).is_err() {
                break;
            }
        }
        thread::sleep(time::Duration::from_millis(1));
    }
}
