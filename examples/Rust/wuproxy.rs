#![crate_name = "wuproxy"]

extern crate zmq;

fn main() {
    let context = zmq::Context::new();

    let mut frontend = context.socket(zmq::XSUB).unwrap();
    assert!(frontend.connect("tcp://192.168.55.210:5556").is_ok());

    let mut backend = context.socket(zmq::XPUB).unwrap();
    assert!(backend.bind("tcp://10.1.1.0:8100").is_ok());

    zmq::proxy(&mut frontend, &mut backend).unwrap();
}
