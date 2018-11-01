extern crate zmq;

use std::thread;
use std::time::Duration;

fn main() {
    let context = zmq::Context::new();

    let subscriber = context.socket(zmq::SUB).unwrap();
    assert!(subscriber.connect("tcp://localhost:5562").is_ok());
    assert!(subscriber.set_subscribe(b"").is_ok());

    thread::sleep(Duration::from_secs(1));

    // socket that receives messages
    let sync_client = context.socket(zmq::REQ).unwrap();
    sync_client.connect("tcp://localhost:5561").unwrap();

    assert!(sync_client.send_str("", 0).is_ok());

    // wait for synchronization reply
    let _ = sync_client.recv_string(0).unwrap().unwrap();

    // Get our updates and report how many we got
    let mut n = 0;
    let mut done = false;
    while !done {
        let msg = subscriber.recv_string(0).unwrap().unwrap_or("".to_string());
        if msg == "Rhubarb" {
            n += 1;
        } else {
            done = msg == "END";
        }
    }
    println!("Received {} updates", n);

}