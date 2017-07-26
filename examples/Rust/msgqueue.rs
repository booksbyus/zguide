#![crate_name = "msgqueue"]

extern crate zmq;

fn main() {
    let context = zmq::Context::new();

    let mut frontend = context.socket(zmq::ROUTER).unwrap();
    assert!(frontend.bind("tcp://*:5559").is_ok());

    let mut backend = context.socket(zmq::DEALER).unwrap();
    assert!(backend.bind("tcp://*:5560").is_ok());

    zmq::proxy(&mut frontend, &mut backend).unwrap();
}
