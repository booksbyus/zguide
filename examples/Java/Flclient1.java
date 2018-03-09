import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.PollItem;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMsg;

//  Freelance client - Model 1
//  Uses REQ socket to query one or more services
public class flclient1
{
    private static final int REQUEST_TIMEOUT = 1000;
    private static final int MAX_RETRIES = 3;       //  Before we abandon

    private static ZMsg tryRequest (ZContext ctx, String endpoint, ZMsg request)
    {
        System.out.printf("I: trying echo service at %s...\n", endpoint);
        Socket client = ctx.createSocket(ZMQ.REQ);
        client.connect(endpoint);

        //  Send request, wait safely for reply
        ZMsg msg = request.duplicate();
        msg.send(client);
        PollItem[] items = { new PollItem(client, ZMQ.Poller.POLLIN) };
        ZMQ.poll(items, REQUEST_TIMEOUT);
        ZMsg reply = null;
        if (items[0].isReadable())
            reply = ZMsg.recvMsg(client);

        //  Close socket in any case, we're done with it now
        ctx.destroySocket(client);
        return reply;
    }
    //  .split client task
    //  The client uses a Lazy Pirate strategy if it only has one server to talk
    //  to. If it has two or more servers to talk to, it will try each server just
    //  once:

    public static void main (String[] argv)
    {
        ZContext ctx = new ZContext();
        ZMsg request = new ZMsg();
        request.add("Hello world");
        ZMsg reply = null;

        int endpoints = argv.length;
        if (endpoints == 0)
            System.out.printf ("I: syntax: flclient1 <endpoint> ...\n");
        else
        if (endpoints == 1) {
            //  For one endpoint, we retry N times
            int retries;
            for (retries = 0; retries < MAX_RETRIES; retries++) {
                String endpoint = argv [0];
                reply = tryRequest(ctx, endpoint, request);
                if (reply != null)
                    break;          //  Successful
                System.out.printf("W: no response from %s, retrying...\n", endpoint);
            }
        }
        else {
            //  For multiple endpoints, try each at most once
            int endpointNbr;
            for (endpointNbr = 0; endpointNbr < endpoints; endpointNbr++) {
                String endpoint = argv[endpointNbr];
                reply = tryRequest (ctx, endpoint, request);
                if (reply != null)
                    break;          //  Successful
                System.out.printf ("W: no response from %s\n", endpoint);
            }
        }
        if (reply != null) {
            System.out.printf ("Service is running OK\n");
            reply.destroy();
        }
        request.destroy();;
        ctx.destroy();
    }

}
