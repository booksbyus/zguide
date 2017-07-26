#![crate_name = "hwclient"]

extern crate zmq;

fn main() {
    println!("Connecting to hello world server...");
    let context = zmq::Context::new();
    let requester = context.socket(zmq::REQ).unwrap();
    assert!(requester.connect("tcp://localhost:5555").is_ok());

    for request_nbr in 0..10 {
        let buffer = &mut [0; 10];
        println!("Sending Hello {:?}...", request_nbr);
        requester.send_str("Hello", 0).unwrap();
        requester.recv_into(buffer, 0).unwrap();
        println!("Received World {:?}", request_nbr);
    }
}
