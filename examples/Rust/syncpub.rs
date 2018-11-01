extern crate zmq;

fn main() {
    const SUBSCRIBERS_EXPECTED: usize = 10;
    let context = zmq::Context::new();

    // socket that talks to clients
    let publisher = context.socket(zmq::PUB).unwrap();
    // Set the high-water mark to 10k messages
    assert!(publisher.set_sndhwm(1_000_100).is_ok());
    publisher.bind("tcp://*:5562").unwrap();

    // socket that receives messages
    let sync_service = context.socket(zmq::REP).unwrap();
    sync_service.bind("tcp://*:5561").unwrap();

    println!("Waiting for subscribers");
    let mut num_subscribers = 0;
    while num_subscribers < SUBSCRIBERS_EXPECTED {
        let _ = sync_service.recv_string(0).unwrap().unwrap();
        assert!(sync_service.send_str("", 0).is_ok());
        num_subscribers += 1;
    }
    //  Now broadcast exactly 1M updates followed by END
    println!("Broadcasting messages");
    for _ in 0..1_000_000 {
        assert!(publisher.send_str("Rhubarb", 0).is_ok());
    }
    assert!(publisher.send_str("END", 0).is_ok());
}