fn main() {
    let (major, minor, patch) = zmq::version();
    println!("Current 0MQ version is {}.{}.{}", major, minor, patch);
}
