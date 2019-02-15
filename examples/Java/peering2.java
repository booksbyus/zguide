package guide;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;

import org.zeromq.*;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;

//  Broker peering simulation (part 2)
//  Prototypes the request-reply flow

public class peering2
{

    private static final int    NBR_CLIENTS  = 10;
    private static final int    NBR_WORKERS  = 3;
    private static final String WORKER_READY = "\001"; //  Signals worker is ready

    //  Our own name; in practice this would be configured per node
    private static String self;

    //  The client task does a request-reply dialog using a standard
    //  synchronous REQ socket:
    private static class client_task extends Thread
    {
        @Override
        public void run()
        {
            try (ZContext ctx = new ZContext()) {
                Socket client = ctx.createSocket(SocketType.REQ);
                client.connect(String.format("ipc://%s-localfe.ipc", self));

                while (true) {
                    //  Send request, get reply
                    client.send("HELLO", 0);
                    String reply = client.recvStr(0);
                    if (reply == null)
                        break; //  Interrupted
                    System.out.printf("Client: %s\n", reply);
                    try {
                        Thread.sleep(1000);
                    }
                    catch (InterruptedException e) {
                    }
                }
            }
        }
    }

    //  The worker task plugs into the LRU routing dialog using a REQ
    //  socket:

    private static class worker_task extends Thread
    {
        @Override
        public void run()
        {
            try (ZContext ctx = new ZContext()) {
                Socket worker = ctx.createSocket(SocketType.REQ);
                worker.connect(String.format("ipc://%s-localbe.ipc", self));

                //  Tell broker we're ready for work
                ZFrame frame = new ZFrame(WORKER_READY);
                frame.send(worker, 0);

                while (true) {
                    //  Send request, get reply
                    ZMsg msg = ZMsg.recvMsg(worker, 0);
                    if (msg == null)
                        break; //  Interrupted
                    msg.getLast().print("Worker: ");
                    msg.getLast().reset("OK");
                    msg.send(worker);

                }
            }
        }
    }

    //  The main task begins by setting-up its frontend and backend sockets
    //  and then starting its client and worker tasks:
    public static void main(String[] argv)
    {
        //  First argument is this broker's name
        //  Other arguments are our peers' names
        //
        if (argv.length < 1) {
            System.out.println("syntax: peering2 me {you}");
            System.exit(-1);
        }
        self = argv[0];
        System.out.printf("I: preparing broker at %s\n", self);
        Random rand = new Random(System.nanoTime());

        try (ZContext ctx = new ZContext()) {
            //  Bind cloud frontend to endpoint
            Socket cloudfe = ctx.createSocket(SocketType.ROUTER);
            cloudfe.setIdentity(self.getBytes(ZMQ.CHARSET));
            cloudfe.bind(String.format("ipc://%s-cloud.ipc", self));

            //  Connect cloud backend to all peers
            Socket cloudbe = ctx.createSocket(SocketType.ROUTER);
            cloudbe.setIdentity(self.getBytes(ZMQ.CHARSET));
            int argn;
            for (argn = 1; argn < argv.length; argn++) {
                String peer = argv[argn];
                System.out.printf(
                    "I: connecting to cloud forintend at '%s'\n", peer
                );
                cloudbe.connect(String.format("ipc://%s-cloud.ipc", peer));
            }

            //  Prepare local frontend and backend
            Socket localfe = ctx.createSocket(SocketType.ROUTER);
            localfe.bind(String.format("ipc://%s-localfe.ipc", self));
            Socket localbe = ctx.createSocket(SocketType.ROUTER);
            localbe.bind(String.format("ipc://%s-localbe.ipc", self));

            //  Get user to tell us when we can start
            System.out.println("Press Enter when all brokers are started: ");
            try {
                System.in.read();
            }
            catch (IOException e) {
                e.printStackTrace();
            }

            //  Start local workers
            int worker_nbr;
            for (worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++)
                new worker_task().start();

            //  Start local clients
            int client_nbr;
            for (client_nbr = 0; client_nbr < NBR_CLIENTS; client_nbr++)
                new client_task().start();

            //  Here we handle the request-reply flow. We're using the LRU
            //  approach to poll workers at all times, and clients only when
            //  there are one or more workers available.

            //  Least recently used queue of available workers
            int capacity = 0;
            ArrayList<ZFrame> workers = new ArrayList<ZFrame>();

            Poller backends = ctx.createPoller(2);
            backends.register(localbe, Poller.POLLIN);
            backends.register(cloudbe, Poller.POLLIN);

            Poller frontends = ctx.createPoller(2);
            frontends.register(localfe, Poller.POLLIN);
            frontends.register(cloudfe, Poller.POLLIN);

            while (true) {
                //  First, route any waiting replies from workers

                //  If we have no workers anyhow, wait indefinitely
                int rc = backends.poll(capacity > 0 ? 1000 : -1);
                if (rc == -1)
                    break; //  Interrupted
                //  Handle reply from local worker
                ZMsg msg = null;
                if (backends.pollin(0)) {
                    msg = ZMsg.recvMsg(localbe);
                    if (msg == null)
                        break; //  Interrupted
                    ZFrame address = msg.unwrap();
                    workers.add(address);
                    capacity++;

                    //  If it's READY, don't route the message any further
                    ZFrame frame = msg.getFirst();
                    String frameData = new String(frame.getData(), ZMQ.CHARSET);
                    if (frameData.equals(WORKER_READY)) {
                        msg.destroy();
                        msg = null;
                    }
                }
                //  Or handle reply from peer broker
                else if (backends.pollin(1)) {
                    msg = ZMsg.recvMsg(cloudbe);
                    if (msg == null)
                        break; //  Interrupted
                    //  We don't use peer broker address for anything
                    ZFrame address = msg.unwrap();
                    address.destroy();
                }
                //  Route reply to cloud if it's addressed to a broker
                for (argn = 1; msg != null && argn < argv.length; argn++) {
                    byte[] data = msg.getFirst().getData();
                    if (argv[argn].equals(new String(data, ZMQ.CHARSET))) {
                        msg.send(cloudfe);
                        msg = null;
                    }
                }
                //  Route reply to client if we still need to
                if (msg != null)
                    msg.send(localfe);

                //  Now we route as many client requests as we have worker
                //  capacity for. We may reroute requests from our local
                //  frontend, but not from // the cloud frontend. We reroute
                //  randomly now, just to test things out. In the next version
                //  we'll do this properly by calculating cloud capacity://

                while (capacity > 0) {
                    rc = frontends.poll(0);
                    assert (rc >= 0);
                    int reroutable = 0;
                    //  We'll do peer brokers first, to prevent starvation
                    if (frontends.pollin(1)) {
                        msg = ZMsg.recvMsg(cloudfe);
                        reroutable = 0;
                    }
                    else if (frontends.pollin(0)) {
                        msg = ZMsg.recvMsg(localfe);
                        reroutable = 1;
                    }
                    else break; //  No work, go back to backends

                    //  If reroutable, send to cloud 20% of the time
                    //  Here we'd normally use cloud status information
                    if (reroutable != 0 &&
                        argv.length > 1 &&
                        rand.nextInt(5) == 0) {
                        //  Route to random broker peer
                        int random_peer = rand.nextInt(argv.length - 1) + 1;
                        msg.push(argv[random_peer]);
                        msg.send(cloudbe);
                    }
                    else {
                        ZFrame frame = workers.remove(0);
                        msg.wrap(frame);
                        msg.send(localbe);
                        capacity--;
                    }
                }
            }
            //  When we're done, clean up properly
            while (workers.size() > 0) {
                ZFrame frame = workers.remove(0);
                frame.destroy();
            }
        }
    }
}
