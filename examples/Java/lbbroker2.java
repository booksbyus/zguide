package guide;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

import org.zeromq.*;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;

/**
 * Load-balancing broker
 * Demonstrates use of the high level API
 */
public class lbbroker2
{
    private static final int NBR_CLIENTS  = 10;
    private static final int NBR_WORKERS  = 3;
    private static byte[]    WORKER_READY = { '\001' }; //  Signals worker is ready

    /**
     * Basic request-reply client using REQ socket
     */
    private static class ClientTask implements ZThread.IDetachedRunnable
    {
        @Override
        public void run(Object[] args)
        {
            //  Prepare our context and sockets
            try (ZContext context = new ZContext()) {
                Socket client = context.createSocket(SocketType.REQ);
                ZHelper.setId(client); //  Set a printable identity

                client.connect("ipc://frontend.ipc");

                //  Send request, get reply
                client.send("HELLO");
                String reply = client.recvStr();
                System.out.println("Client: " + reply);
            }
        }
    }

    /**
     * Worker using REQ socket to do load-balancing
     */
    private static class WorkerTask implements ZThread.IDetachedRunnable
    {
        @Override
        public void run(Object[] args)
        {
            //  Prepare our context and sockets
            try (ZContext context = new ZContext()) {
                Socket worker = context.createSocket(SocketType.REQ);
                ZHelper.setId(worker); //  Set a printable identity

                worker.connect("ipc://backend.ipc");

                //  Tell backend we're ready for work
                ZFrame frame = new ZFrame(WORKER_READY);
                frame.send(worker, 0);

                while (true) {
                    ZMsg msg = ZMsg.recvMsg(worker);
                    if (msg == null)
                        break;

                    msg.getLast().reset("OK");
                    msg.send(worker);
                }
            }
        }
    }

    /**
     * This is the main task. This has the identical functionality to
     * the previous lbbroker example but uses higher level classes to start child threads
     * to hold the list of workers, and to read and send messages:
     */
    public static void main(String[] args)
    {
        //  Prepare our context and sockets
        try (ZContext context = new ZContext()) {
            Socket frontend = context.createSocket(SocketType.ROUTER);
            Socket backend = context.createSocket(SocketType.ROUTER);
            frontend.bind("ipc://frontend.ipc");
            backend.bind("ipc://backend.ipc");

            int clientNbr;
            for (clientNbr = 0; clientNbr < NBR_CLIENTS; clientNbr++)
                ZThread.start(new ClientTask());

            for (int workerNbr = 0; workerNbr < NBR_WORKERS; workerNbr++)
                ZThread.start(new WorkerTask());

            //  Queue of available workers
            Queue<ZFrame> workerQueue = new LinkedList<ZFrame>();

            //  Here is the main loop for the load-balancer. It works the same
            //  way as the previous example, but is a lot shorter because ZMsg
            //  class gives us an API that does more with fewer calls:

            while (!Thread.currentThread().isInterrupted()) {
                //  Initialize poll set
                Poller items = context.createPoller(2);

                //  Always poll for worker activity on backend
                items.register(backend, Poller.POLLIN);

                //  Poll front-end only if we have available workers
                if (workerQueue.size() > 0)
                    items.register(frontend, Poller.POLLIN);

                if (items.poll() < 0)
                    break; //  Interrupted

                //  Handle worker activity on backend
                if (items.pollin(0)) {

                    ZMsg msg = ZMsg.recvMsg(backend);
                    if (msg == null)
                        break; //  Interrupted

                    ZFrame identity = msg.unwrap();
                    //  Queue worker address for LRU routing
                    workerQueue.add(identity);

                    //  Forward message to client if it's not a READY
                    ZFrame frame = msg.getFirst();
                    if (Arrays.equals(frame.getData(), WORKER_READY))
                        msg.destroy();
                    else msg.send(frontend);
                }

                if (items.pollin(1)) {
                    //  Get client request, route to first available worker
                    ZMsg msg = ZMsg.recvMsg(frontend);
                    if (msg != null) {
                        msg.wrap(workerQueue.poll());
                        msg.send(backend);
                    }
                }
            }
        }
    }
}
