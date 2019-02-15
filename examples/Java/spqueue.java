package guide;

import java.util.ArrayList;

import org.zeromq.*;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;

//
// Simple Pirate queue
// This is identical to load-balancing  pattern, with no reliability mechanisms
// at all. It depends on the client for recovery. Runs forever.
//
public class spqueue
{

    private final static String WORKER_READY = "\001"; //  Signals worker is ready

    public static void main(String[] args)
    {
        try (ZContext ctx = new ZContext()) {
            Socket frontend = ctx.createSocket(SocketType.ROUTER);
            Socket backend = ctx.createSocket(SocketType.ROUTER);
            frontend.bind("tcp://*:5555"); //  For clients
            backend.bind("tcp://*:5556"); //  For workers

            //  Queue of available workers
            ArrayList<ZFrame> workers = new ArrayList<ZFrame>();

            Poller poller = ctx.createPoller(2);
            poller.register(backend, Poller.POLLIN);
            poller.register(frontend, Poller.POLLIN);

            //  The body of this example is exactly the same as lruqueue2.
            while (true) {
                boolean workersAvailable = workers.size() > 0;
                int rc = poller.poll(-1);

                //  Poll frontend only if we have available workers
                if (rc == -1)
                    break; //  Interrupted

                //  Handle worker activity on backend
                if (poller.pollin(0)) {
                    //  Use worker address for LRU routing
                    ZMsg msg = ZMsg.recvMsg(backend);
                    if (msg == null)
                        break; //  Interrupted
                    ZFrame address = msg.unwrap();
                    workers.add(address);

                    //  Forward message to client if it's not a READY
                    ZFrame frame = msg.getFirst();
                    if (new String(frame.getData(), ZMQ.CHARSET).equals(WORKER_READY))
                        msg.destroy();
                    else msg.send(frontend);
                }
                if (workersAvailable && poller.pollin(1)) {
                    //  Get client request, route to first available worker
                    ZMsg msg = ZMsg.recvMsg(frontend);
                    if (msg != null) {
                        msg.wrap(workers.remove(0));
                        msg.send(backend);
                    }
                }
            }

            //  When we're done, clean up properly
            while (workers.size() > 0) {
                ZFrame frame = workers.remove(0);
                frame.destroy();
            }

            workers.clear();
        }
    }
}
