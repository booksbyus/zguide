#![crate_name = "mtrelay"]

extern crate zmq;

use std::thread;

fn step1(context: &zmq::Context) {
    let xmitter = context.socket(zmq::PAIR).unwrap();
    assert!(xmitter.connect("inproc://step2").is_ok());
    println!("Step 1 ready, signaling step 2");
    xmitter.send_str("READY", 0).unwrap();
}

fn step2(context: &zmq::Context) {
    let receiver = context.socket(zmq::PAIR).unwrap();
    assert!(receiver.bind("inproc://step2").is_ok());

    let ncontext = context.clone();
    thread::spawn(move || step1(&ncontext));

    let _ = receiver.recv_string(0).unwrap().unwrap();

    let xmitter = context.socket(zmq::PAIR).unwrap();
    assert!(xmitter.connect("inproc://step3").is_ok());
    println!("Step 2 ready, signaling step 3");
    xmitter.send_str("READY", 0).unwrap();
}

fn main() {
    let context = zmq::Context::new();

    let receiver = context.socket(zmq::PAIR).unwrap();
    assert!(receiver.bind("inproc://step3").is_ok());

    let ncontext = context.clone();
    thread::spawn(move || step2(&ncontext));

    let _ = receiver.recv_string(0).unwrap().unwrap();

    println!("Test successful!");
}
