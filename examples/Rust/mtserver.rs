use std::{thread, time};

fn worker_routine(context: &zmq::Context) {
    let receiver = context.socket(zmq::REP).unwrap();
    assert!(receiver.connect("inproc://workers").is_ok());

    loop {
        let string = receiver.recv_string(0).unwrap().unwrap();
        println!("Received request: {}", string);
        thread::sleep(time::Duration::from_secs(1));
        receiver.send("World", 0).unwrap();
    }
}

fn main() {
    let context = zmq::Context::new();

    let clients = context.socket(zmq::ROUTER).unwrap();
    assert!(clients.bind("tcp://*:5555").is_ok());

    let workers = context.socket(zmq::DEALER).unwrap();
    assert!(workers.bind("inproc://workers").is_ok());

    for _ in 0..5 {
        let ncontext = context.clone();
        thread::spawn(move || worker_routine(&ncontext));
    }

    zmq::proxy(&clients, &workers).unwrap();
}
