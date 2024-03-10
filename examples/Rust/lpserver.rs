use rand::{thread_rng, Rng};
use std::time::Duration;

fn main() {
    let context = zmq::Context::new();
    let server = context.socket(zmq::REP).unwrap();
    server.bind("tcp://*:5555").unwrap();

    let mut i = 0;
    loop {
        i += 1;
        let request = server.recv_msg(0).unwrap();
        println!("Got Request: {request:?}");
        server.send(request, 0).unwrap();
        std::thread::sleep(Duration::from_secs(1));

        if (i > 3) && (thread_rng().gen_range(0..3) == 0) {
            // simulate a crash
            println!("Oh no! Server crashed.");
            break;
        }
        if (i > 3) && (thread_rng().gen_range(0..3) == 0) {
            // simulate overload
            println!("Server is busy.");
            std::thread::sleep(Duration::from_secs(2));
        }
    }
}
