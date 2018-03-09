import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.PollItem;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMsg;
import org.zeromq.ZMQ.Poller;

import java.util.Random;

//
//Asynchronous client-to-server (DEALER to ROUTER)
//
//While this example runs in a single process, that is just to make
//it easier to start and stop the example. Each task has its own
//context and conceptually acts as a separate process.


public class asyncsrv
{
    //---------------------------------------------------------------------
    //This is our client task
    //It connects to the server, and then sends a request once per second
    //It collects responses as they arrive, and it prints them out. We will
    //run several client tasks in parallel, each with a different random ID.

    private static Random rand = new Random(System.nanoTime());

    private static class client_task implements Runnable {

        public void run() {
            ZContext ctx = new ZContext();
            Socket client = ctx.createSocket(ZMQ.DEALER);

            //  Set random identity to make tracing easier
            String identity = String.format("%04X-%04X", rand.nextInt(), rand.nextInt());
            client.setIdentity(identity.getBytes());
            client.connect("tcp://localhost:5570");

            PollItem[] items = new PollItem[] { new PollItem(client, Poller.POLLIN) };

            int requestNbr = 0;
            while (!Thread.currentThread().isInterrupted()) {
                //  Tick once per second, pulling in arriving messages
                for (int centitick = 0; centitick < 100; centitick++) {
                    ZMQ.poll(items, 10);
                    if (items[0].isReadable()) {
                        ZMsg msg = ZMsg.recvMsg(client);
                        msg.getLast().print(identity);
                        msg.destroy();
                    }
                }
                client.send(String.format("request #%d", ++requestNbr), 0);
            }
            ctx.destroy();
        }
    }

    //This is our server task.
    //It uses the multithreaded server model to deal requests out to a pool
    //of workers and route replies back to clients. One worker can handle
    //one request at a time but one client can talk to multiple workers at
    //once.

    private static class server_task implements Runnable {
        public void run() {
            ZContext ctx = new ZContext();

            //  Frontend socket talks to clients over TCP
            Socket frontend = ctx.createSocket(ZMQ.ROUTER);
            frontend.bind("tcp://*:5570");

            //  Backend socket talks to workers over inproc
            Socket backend = ctx.createSocket(ZMQ.DEALER);
            backend.bind("inproc://backend");

            //  Launch pool of worker threads, precise number is not critical
            for (int threadNbr = 0; threadNbr < 5; threadNbr++)
                new Thread(new server_worker(ctx)).start();

            //  Connect backend to frontend via a proxy
            ZMQ.proxy(frontend, backend, null);

            ctx.destroy();
        }
    }

    //Each worker task works on one request at a time and sends a random number
    //of replies back, with random delays between replies:

    private static class server_worker implements Runnable {
        private ZContext ctx;

        public server_worker(ZContext ctx) {
            this.ctx = ctx;
        }

        public void run() {
            Socket worker = ctx.createSocket(ZMQ.DEALER);
            worker.connect("inproc://backend");

            while (!Thread.currentThread().isInterrupted()) {
                //  The DEALER socket gives us the address envelope and message
                ZMsg msg = ZMsg.recvMsg(worker);
                ZFrame address = msg.pop();
                ZFrame content = msg.pop();
                assert (content != null);
                msg.destroy();

                //  Send 0..4 replies back
                int replies = rand.nextInt(5);
                for (int reply = 0; reply < replies; reply++) {
                    //  Sleep for some fraction of a second
                    try {
                        Thread.sleep(rand.nextInt(1000) + 1);
                    } catch (InterruptedException e) {
                    }
                    address.send(worker, ZFrame.REUSE + ZFrame.MORE);
                    content.send(worker, ZFrame.REUSE);
                }
                address.destroy();
                content.destroy();
            }
            ctx.destroy();
        }
    }

    //The main thread simply starts several clients, and a server, and then
    //waits for the server to finish.

    public static void main(String[] args) throws Exception {
        ZContext ctx = new ZContext();
        new Thread(new client_task()).start();
        new Thread(new client_task()).start();
        new Thread(new client_task()).start();
        new Thread(new server_task()).start();

        //  Run for 5 seconds then quit
        Thread.sleep(5 * 1000);
        ctx.destroy();
    }
}
