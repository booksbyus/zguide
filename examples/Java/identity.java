package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZContext;

/**
 * Demonstrate identities as used by the request-reply pattern.
 */
public class identity
{
    public static void main(String[] args) throws InterruptedException
    {
        try (ZContext context = new ZContext()) {
            Socket sink = context.createSocket(SocketType.ROUTER);
            sink.bind("inproc://example");

            //  First allow 0MQ to set the identity, [00] + random 4byte
            Socket anonymous = context.createSocket(SocketType.REQ);

            anonymous.connect("inproc://example");
            anonymous.send("ROUTER uses a generated UUID", 0);
            ZHelper.dump(sink);

            //  Then set the identity ourself
            Socket identified = context.createSocket(SocketType.REQ);
            identified.setIdentity("Hello".getBytes(ZMQ.CHARSET));
            identified.connect("inproc://example");
            identified.send("ROUTER socket uses REQ's socket identity", 0);
            ZHelper.dump(sink);
        }
    }
}
