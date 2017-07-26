#![crate_name = "wuserver"]

extern crate rand;
extern crate zmq;

use rand::Rng;

fn main() {
    let context = zmq::Context::new();
    let publisher = context.socket(zmq::PUB).unwrap();
    assert!(publisher.bind("tcp://*:5556").is_ok());

    let mut rng = rand::weak_rng();

    loop {
        let zipcode = rng.gen_range(0, 100000);
        let temperature = rng.gen_range(-80, 135);
        let relhumidity = rng.gen_range(10, 60);

        let update = format!("{:05} {} {}", zipcode, temperature, relhumidity);
        publisher.send_str(&update, 0).unwrap();
    }
}
