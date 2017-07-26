#![crate_name = "tasksink"]

extern crate zmq;

use std::io::{self, Write};
use std::time::Instant;

fn main() {
    let context = zmq::Context::new();
    let receiver = context.socket(zmq::PULL).unwrap();
    assert!(receiver.bind("tcp://*:5558").is_ok());

    let _ = receiver.recv_string(0).unwrap();

    let start_time = Instant::now();

    for task_nbr in 0..100 {
        let _ = receiver.recv_string(0).unwrap();
        if task_nbr % 10 == 0 {
            println!(":");
        } else {
            println!(".");
        }
        let _ = io::stdout().flush();
    }

    println!("Total elapsed time: {:?} msec", start_time.elapsed());

    let control = context.socket(zmq::PUB).unwrap();
    assert!(control.bind("tcp://*:5559").is_ok());
    control.send_str("kill", 0).unwrap();
}
