/**
 *  Lazy Pirate client
 *  Use zmq_poll to do a safe request-reply
 *  To run, start lpserver and then randomly kill/restart it
 *
 *  @author Arkadiusz Orzechowski <aorzecho@gmail.com>
 */
import org.zeromq.ZMQ;

public class lpclient {
    public static int REQUEST_TIMEOUT = 2500; // msecs, (> 1000!)
    public static int REQUEST_RETRIES = 3; // before we abandon
    public static String SERVER_ENDPOINT = "tcp://localhost:5555";

    public static void main(String[] args) throws InterruptedException {
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket client = context.socket(ZMQ.REQ);
        System.out.println("Connecting to server...");
        client.connect(SERVER_ENDPOINT);

        int sequence = 0;
        int retriesLeft = REQUEST_RETRIES;
        while (retriesLeft > 0) {
            // We send a request, then we work to get a reply
            String requestString = ++sequence + " ";
            byte[] request = requestString.getBytes();
            request[request.length - 1] = 0; // Sets the last byte to 0
            client.send(request, 0);

            boolean expectReply = true;
            while (expectReply) {
                // Poll socket for a reply, with timeout
                ZMQ.Poller items = context.poller();
                items.register(client, ZMQ.Poller.POLLIN);
                long rc = items.poll(REQUEST_TIMEOUT * 1000);
                if (items.pollin(0)) {
                    final byte[] reply = client.recv(0);
                    final String replyString = new String(reply).trim();
                    int replySequence = -1;
                    try {
                        replySequence = Integer.parseInt(replyString);
                    } catch (Exception ignoreItNow) {
                    }
                    if (replySequence == sequence) {
                        System.out.printf("I: server replied OK (%d)\n",
                                replySequence);
                        retriesLeft = REQUEST_RETRIES;
                        expectReply = false;
                    } else {
                        System.out.printf(
                                "E: malformed reply from server: (%s)\n",
                                replyString);
                    }
                } else {
                    System.out
                            .println("W: no response from server, retrying...");
                    // Old socket is confused; close it and open a new one
                    client.setLinger(0); // drop pending messages immediately 
                                         // on close
                    client.close();
                    items.unregister(client);
                    if (--retriesLeft == 0) {
                        System.out
                          .println("E: server seems to be offline, abandoning");
                        break;
                    }
                    System.out.println("I: reconnecting to server...");
                    client = context.socket(ZMQ.REQ);
                    client.connect(SERVER_ENDPOINT);
                    // Send request again, on new socket
                    client.send(request, 0);
                }
            }
        }
        client.close();
        context.term();
    }
}
