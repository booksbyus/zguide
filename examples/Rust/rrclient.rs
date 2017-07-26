#![crate_name = "rrclient"]

extern crate zmq;

fn main() {
    let context = zmq::Context::new();

    let requester = context.socket(zmq::REQ).unwrap();
    assert!(requester.connect("tcp://localhost:5559").is_ok());

    for request_nbr in 0..10 {
        requester.send_str("Hello", 0).unwrap();
        let string = requester.recv_string(0).unwrap().unwrap();
        println!("Received reply {} {}", request_nbr, string);
    }
}
