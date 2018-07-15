package com.imatix.zguide.ex1_4;

import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

import java.util.stream.IntStream;

import static com.imatix.zguide.commons.ZConstants.URL.TCP;
import static java.lang.String.format;
import static org.zeromq.ZMQ.REQ;
import static org.zeromq.ZMQ.context;

/**
 * Hello World client in Java
 * Connects REQ socket to tcp://*:5555
 * Sends "Hello" to server, expects "World"
 *
 * @since 1.0
 */

public class HwClient {

    public static void main(String[] args) {
        Context context = context(1);
        // Socket to talk to Server
        Socket requester = context.socket(REQ);
        requester.connect(TCP);
        IntStream.range(0, 10)
        .forEach(reqNum -> {
            System.out.println(format("Sending Hello %s", reqNum));
            requester.send("Hello".getBytes(), 0);
            byte[] reply = requester.recv(0);
            System.out.println(format("Received %s : %s", new String(reply), reqNum));
        });
        requester.close();
        context.term();
    }
}
