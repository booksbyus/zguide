import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

/**
 * Demonstrate identities as used by the request-reply pattern.
 */
public class Identity {

    public static void main (String[] args) throws InterruptedException {

        Context context = ZMQ.context(1);
        Socket sink = context.socket(ZMQ.ROUTER);
        sink.bind("inproc://example");

        //  First allow 0MQ to set the Identity, [00] + random 4byte
        Socket anonymous = context.socket(ZMQ.REQ);

        anonymous.connect("inproc://example");
        anonymous.send ("ROUTER uses a generated 5 byte Identity",0);
        ZHelper.dump (sink);

        //  Then set the Identity ourself
        Socket identified = context.socket(ZMQ.REQ);
        identified.setIdentity("PEER2".getBytes ());
        identified.connect ("inproc://example");
        identified.send("ROUTER uses REQ's socket Identity", 0);
        ZHelper.dump (sink);

        sink.close ();
        anonymous.close ();
        identified.close();
        context.term();

    }
}
