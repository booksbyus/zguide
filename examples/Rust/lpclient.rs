use std::convert::TryInto;

const REQUEST_TIMEOUT: i64 = 2500;
const REQUEST_RETRIES: usize = 3;
const SERVER_ENDPOINT: &'static str = "tcp://localhost:5555";

fn connect(context: &zmq::Context) -> zmq::Socket {
    let socket = context.socket(zmq::REQ).unwrap();
    socket.set_linger(0).unwrap();
    socket.connect(SERVER_ENDPOINT).unwrap();
    socket
}

fn reconnect(context: &zmq::Context, socket: zmq::Socket) -> zmq::Socket {
    drop(socket);
    connect(context)
}

fn main() {
    let mut retries_left = REQUEST_RETRIES;

    let context = zmq::Context::new();
    let mut socket = connect(&context);

    let mut i = 0i64;
    loop {
        let value = i.to_le_bytes();
        i += 1;
        while retries_left > 0 {
            retries_left -= 1;

            let request = zmq::Message::from(&value.as_slice());
            println!("Sending request {request:?}");
            socket.send(request, 0).unwrap();

            if socket.poll(zmq::POLLIN, REQUEST_TIMEOUT).unwrap() != 0 {
                let reply = socket.recv_msg(0).unwrap();
                let reply_content =
                    TryInto::<[u8; 8]>::try_into(&*reply).and_then(|x| Ok(i64::from_le_bytes(x)));
                if let Ok(i) = reply_content {
                    println!("Server replied OK {i}");
                    retries_left = REQUEST_RETRIES;
                    break;
                } else {
                    println!("Malformed message: {reply:?}");
                    continue;
                }
            }
            println!("No response from Server");
            println!("Reconnecting server...");
            socket = reconnect(&context, socket);
            println!("Resending Request");
        }

        if retries_left == 0 {
            println!("Server is offline. Goodbye...");
            return;
        }
    }
}
