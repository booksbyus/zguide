package guide;

import java.util.LinkedList;
import java.util.Queue;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZContext;

public class lbbroker
{
    private static final int NBR_CLIENTS = 10;
    private static final int NBR_WORKERS = 3;

    /**
     * Basic request-reply client using REQ socket
     */
    private static class ClientTask extends Thread
    {
        @Override
        public void run()
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
     * While this example runs in a single process, that is just to make
     * it easier to start and stop the example. Each thread has its own
     * context and conceptually acts as a separate process.
     * This is the worker task, using a REQ socket to do load-balancing.
     */
    private static class WorkerTask extends Thread
    {
        @Override
        public void run()
        {
            //  Prepare our context and sockets
            try (ZContext context = new ZContext()) {
                Socket worker = context.createSocket(SocketType.REQ);
                ZHelper.setId(worker); //  Set a printable identity

                worker.connect("ipc://backend.ipc");

                //  Tell backend we're ready for work
                worker.send("READY");

                while (!Thread.currentThread().isInterrupted()) {
                    String address = worker.recvStr();
                    String empty = worker.recvStr();
                    assert (empty.length() == 0);

                    //  Get request, send reply
                    String request = worker.recvStr();
                    System.out.println("Worker: " + request);

                    worker.sendMore(address);
                    worker.sendMore("");
                    worker.send("OK");
                }
            }
        }
    }

    /**
     * This is the main task. It starts the clients and workers, and then
     * routes requests between the two layers. Workers signal READY when
     * they start; after that we treat them as ready when they reply with
     * a response back to a client. The load-balancing data structure is
     * just a queue of next available workers.
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
                new ClientTask().start();

            for (int workerNbr = 0; workerNbr < NBR_WORKERS; workerNbr++)
                new WorkerTask().start();

            //  Here is the main loop for the least-recently-used queue. It has
            //  two sockets; a frontend for clients and a backend for workers.
            //  It polls the backend in all cases, and polls the frontend only
            //  when there are one or more workers ready. This is a neat way to
            //  use 0MQ's own queues to hold messages we're not ready to process
            //  yet. When we get a client reply, we pop the next available
            //  worker, and send the request to it, including the originating
            //  client identity. When a worker replies, we re-queue that worker,
            //  and we forward the reply to the original client, using the reply
            //  envelope.

            //  Queue of available workers
            Queue<String> workerQueue = new LinkedList<String>();

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

                    //  Queue worker address for LRU routing
                    workerQueue.add(backend.recvStr());

                    //  Second frame is empty
                    String empty = backend.recvStr();
                    assert (empty.length() == 0);

                    //  Third frame is READY or else a client reply address
                    String clientAddr = backend.recvStr();

                    //  If client reply, send rest back to frontend
                    if (!clientAddr.equals("READY")) {

                        empty = backend.recvStr();
                        assert (empty.length() == 0);

                        String reply = backend.recvStr();
                        frontend.sendMore(clientAddr);
                        frontend.sendMore("");
                        frontend.send(reply);

                        if (--clientNbr == 0)
                            break;
                    }
                }

                if (items.pollin(1)) {
                    //  Now get next client request, route to LRU worker
                    //  Client request is [address][empty][request]
                    String clientAddr = frontend.recvStr();

                    String empty = frontend.recvStr();
                    assert (empty.length() == 0);

                    String request = frontend.recvStr();

                    String workerAddr = workerQueue.poll();

                    backend.sendMore(workerAddr);
                    backend.sendMore("");
                    backend.sendMore(clientAddr);
                    backend.sendMore("");
                    backend.send(request);
                }
            }
        }
    }
}
