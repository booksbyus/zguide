#![crate_name = "mspoller"]

extern crate zmq;

fn main() {
    let context = zmq::Context::new();

    let receiver = context.socket(zmq::PULL).unwrap();
    assert!(receiver.connect("tcp://localhost:5557").is_ok());

    let subscriber = context.socket(zmq::SUB).unwrap();
    assert!(subscriber.connect("tcp://localhost:5556").is_ok());
    assert!(subscriber.set_subscribe(b"10001").is_ok());

    let items = &mut [
        receiver.as_poll_item(zmq::POLLIN),
        subscriber.as_poll_item(zmq::POLLIN),
    ];

    loop {
        zmq::poll(items, -1).unwrap();
        if items[0].is_readable() {
            let _ = receiver.recv_msg(0);
        }
        if items[1].is_readable() {
            let _ = subscriber.recv_msg(0);
        }
    }
}
