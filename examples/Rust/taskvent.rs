use rand::Rng;
use std::io::{self, BufRead};
use std::{thread, time};

fn main() {
    let context = zmq::Context::new();
    let sender = context.socket(zmq::PUSH).unwrap();
    assert!(sender.bind("tcp://*:5557").is_ok());

    let sink = context.socket(zmq::PUSH).unwrap();
    assert!(sink.connect("tcp://localhost:5558").is_ok());

    println!("Press Enter when the workers are ready: ");
    let stdin = io::stdin();
    stdin.lock().lines().next();
    println!("Sending tasks to workers...");

    sink.send("0", 0).unwrap();

    let mut rng = rand::thread_rng();

    let mut total_msec = 0;
    for _ in 0..100 {
        let workload = rng.gen_range(1..101);
        total_msec += workload;
        let string = format!("{}", workload);
        sender.send(&string, 0).unwrap();
    }
    println!("Total expected cost: {} msec", total_msec);
    thread::sleep(time::Duration::from_secs(1));
}
