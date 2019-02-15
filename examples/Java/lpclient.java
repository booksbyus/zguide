package guide;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;

//
// Lazy Pirate client
// Use zmq_poll to do a safe request-reply
// To run, start lpserver and then randomly kill/restart it
//

public class lpclient
{

    private final static int    REQUEST_TIMEOUT = 2500;                  //  msecs, (> 1000!)
    private final static int    REQUEST_RETRIES = 3;                     //  Before we abandon
    private final static String SERVER_ENDPOINT = "tcp://localhost:5555";

    public static void main(String[] argv)
    {
        try (ZContext ctx = new ZContext()) {
            System.out.println("I: connecting to server");
            Socket client = ctx.createSocket(SocketType.REQ);
            assert (client != null);
            client.connect(SERVER_ENDPOINT);

            Poller poller = ctx.createPoller(1);
            poller.register(client, Poller.POLLIN);

            int sequence = 0;
            int retriesLeft = REQUEST_RETRIES;
            while (retriesLeft > 0 && !Thread.currentThread().isInterrupted()) {
                //  We send a request, then we work to get a reply
                String request = String.format("%d", ++sequence);
                client.send(request);

                int expect_reply = 1;
                while (expect_reply > 0) {
                    //  Poll socket for a reply, with timeout
                    int rc = poller.poll(REQUEST_TIMEOUT);
                    if (rc == -1)
                        break; //  Interrupted

                    //  Here we process a server reply and exit our loop if the
                    //  reply is valid. If we didn't a reply we close the client
                    //  socket and resend the request. We try a number of times
                    //  before finally abandoning:

                    if (poller.pollin(0)) {
                        //  We got a reply from the server, must match
                        //  getSequence
                        String reply = client.recvStr();
                        if (reply == null)
                            break; //  Interrupted
                        if (Integer.parseInt(reply) == sequence) {
                            System.out.printf(
                                "I: server replied OK (%s)\n", reply
                            );
                            retriesLeft = REQUEST_RETRIES;
                            expect_reply = 0;
                        }
                        else System.out.printf(
                            "E: malformed reply from server: %s\n", reply
                        );

                    }
                    else if (--retriesLeft == 0) {
                        System.out.println(
                            "E: server seems to be offline, abandoning\n"
                        );
                        break;
                    }
                    else {
                        System.out.println(
                            "W: no response from server, retrying\n"
                        );
                        //  Old socket is confused; close it and open a new one
                        poller.unregister(client);
                        ctx.destroySocket(client);
                        System.out.println("I: reconnecting to server\n");
                        client = ctx.createSocket(SocketType.REQ);
                        client.connect(SERVER_ENDPOINT);
                        poller.register(client, Poller.POLLIN);
                        //  Send request again, on new socket
                        client.send(request);
                    }
                }
            }
        }
    }
}
