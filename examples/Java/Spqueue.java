import java.util.ArrayList;

import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.PollItem;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMsg;

//
// Simple Pirate queue
// This is identical to the LRU pattern, with no reliability mechanisms
// at all. It depends on the client for recovery. Runs forever.
//
public class spqueue {

    private final static String WORKER_READY  = "\001";      //  Signals worker is ready
    public static void main(String[] args) {
        ZContext ctx = new ZContext ();
        Socket frontend = ctx.createSocket(ZMQ.ROUTER);
        Socket backend = ctx.createSocket(ZMQ.ROUTER);
        frontend.bind("tcp://*:5555");    //  For clients
        backend.bind("tcp://*:5556");    //  For workers

        //  Queue of available workers
        ArrayList<ZFrame> workers = new ArrayList<ZFrame> ();
        
        //  The body of this example is exactly the same as lruqueue2.
        while (true) {
            PollItem items [] = {
                new PollItem( backend,  Poller.POLLIN ),
                new PollItem( frontend, Poller.POLLIN )
            };
            int rc = ZMQ.poll (items, workers.size() > 0 ? 2 : 1, -1);

            //  Poll frontend only if we have available workers
            if (rc == -1)
                break;              //  Interrupted

            //  Handle worker activity on backend
            if (items [0].isReadable()) {
                //  Use worker address for LRU routing
                ZMsg msg = ZMsg.recvMsg(backend);
                if (msg == null)
                    break;          //  Interrupted
                ZFrame address = msg.unwrap();
                workers.add( address);

                //  Forward message to client if it's not a READY
                ZFrame frame = msg.getFirst();
                if (new String(frame.getData()).equals(WORKER_READY))
                    msg.destroy();
                else
                    msg.send(frontend);
            }
            if (items [1].isReadable()) {
                //  Get client request, route to first available worker
                ZMsg msg = ZMsg.recvMsg (frontend);
                if (msg != null) {
                    msg.wrap (workers.remove(0));
                    msg.send(backend);
                }
            }
        }
        //  When we're done, clean up properly
        while (workers.size()>0) {
            ZFrame frame = workers.remove(0); 
            frame.destroy();
        }
        workers.clear();
        ctx.destroy();
    }

}
