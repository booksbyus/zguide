#![crate_name = "rrbroker"]

extern crate zmq;

fn main() {
    let context = zmq::Context::new();
    let frontend = context.socket(zmq::ROUTER).unwrap();
    let backend = context.socket(zmq::DEALER).unwrap();
    assert!(frontend.bind("tcp://*:5559").is_ok());
    assert!(backend.bind("tcp://*:5560").is_ok());

    let items = &mut [
        frontend.as_poll_item(zmq::POLLIN),
        backend.as_poll_item(zmq::POLLIN),
    ];

    loop {
        zmq::poll(items, -1).unwrap();
        if items[0].is_readable() {
            loop {
                let message = frontend.recv_msg(0).unwrap();
                let more = if frontend.get_rcvmore().unwrap() {
                    zmq::SNDMORE
                } else {
                    0
                };
                backend.send_msg(message, more).unwrap();
                if more == 0 {
                    break;
                };
            }
        }
        if items[1].is_readable() {
            loop {
                let message = backend.recv_msg(0).unwrap();
                let more = if backend.get_rcvmore().unwrap() {
                    zmq::SNDMORE
                } else {
                    0
                };
                frontend.send_msg(message, more).unwrap();
                if more == 0 {
                    break;
                }
            }
        }
    }
}
