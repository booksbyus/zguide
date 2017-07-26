#![crate_name = "psenvsub"]

extern crate zmq;

fn main() {
    let context = zmq::Context::new();
    let subscriber = context.socket(zmq::SUB).unwrap();
    assert!(subscriber.connect("tcp://localhost:5563").is_ok());
    subscriber.set_subscribe(b"B").unwrap();

    loop {
        let address = subscriber.recv_string(0).unwrap().unwrap();
        let contents = subscriber.recv_string(0).unwrap().unwrap();
        println!("[{}] {}", address, contents);
    }
}
