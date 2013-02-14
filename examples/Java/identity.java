import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

/**
 * Demonstrate identities as used by the request-reply pattern.
 */
public class identity {

    public static void main (String[] args) throws InterruptedException {

        Context context = ZMQ.context(1);
        Socket sink = context.socket(ZMQ.ROUTER);
        sink.bind("inproc://example");

        //  First allow 0MQ to set the identity, [00] + random 4byte
        Socket anonymous = context.socket(ZMQ.REQ);

        anonymous.connect("inproc://example");
        anonymous.send ("ROUTER uses a generated UUID",0);
        ZHelper.dump (sink);

        //  Then set the identity ourself
        Socket identified = context.socket(ZMQ.REQ);
        identified.setIdentity("Hello".getBytes ());
        identified.connect ("inproc://example");
        identified.send("ROUTER socket uses REQ's socket identity", 0);
        ZHelper.dump (sink);

        sink.close ();
        anonymous.close ();
        identified.close();
        context.term();

    }
}
