package guide;

import java.util.ArrayList;
import java.util.Random;

import org.zeromq.*;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;

//  Broker peering simulation (part 3)
//  Prototypes the full flow of status and tasks

public class peering3
{

    private static final int    NBR_CLIENTS  = 10;
    private static final int    NBR_WORKERS  = 5;
    // Signals worker is ready
    private static final String WORKER_READY = "\001";

    //  Our own name; in practice this would be configured per node
    private static String self;

    //  This is the client task. It issues a burst of requests and then sleeps
    //  for a few seconds. This simulates sporadic activity; when a number of
    //  clients are active at once, the local workers should be overloaded. The
    //  client uses a REQ socket for requests and also pushes statistics to the
    //  monitor socket:
    private static class client_task extends Thread
    {
        @Override
        public void run()
        {
            try (ZContext ctx = new ZContext()) {
                Socket client = ctx.createSocket(SocketType.REQ);
                client.connect(String.format("ipc://%s-localfe.ipc", self));
                Socket monitor = ctx.createSocket(SocketType.PUSH);
                monitor.connect(String.format("ipc://%s-monitor.ipc", self));
                Random rand = new Random(System.nanoTime());

                Poller poller = ctx.createPoller(1);
                poller.register(client, Poller.POLLIN);

                boolean done = false;
                while (!done) {
                    try {
                        Thread.sleep(rand.nextInt(5) * 1000);
                    }
                    catch (InterruptedException e1) {
                    }
                    int burst = rand.nextInt(15);

                    while (burst > 0) {
                        String taskId = String.format(
                            "%04X", rand.nextInt(10000)
                        );
                        //  Send request, get reply
                        client.send(taskId, 0);

                        //  Wait max ten seconds for a reply, then complain
                        int rc = poller.poll(10 * 1000);
                        if (rc == -1)
                            break; //  Interrupted

                        if (poller.pollin(0)) {
                            String reply = client.recvStr(0);
                            if (reply == null)
                                break; //  Interrupted
                            //  Worker is supposed to answer us with our task id
                            assert (reply.equals(taskId));
                            monitor.send(String.format("%s", reply), 0);
                        }
                        else {
                            monitor.send(
                                String.format(
                                    "E: CLIENT EXIT - lost task %s", taskId
                                ),
                            0);
                            done = true;
                            break;
                        }
                        burst--;
                    }
                }
            }
        }
    }

    //  This is the worker task, which uses a REQ socket to plug into the LRU
    //  router. It's the same stub worker task you've seen in other examples:

    private static class worker_task extends Thread
    {
        @Override
        public void run()
        {
            Random rand = new Random(System.nanoTime());
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

                    //  Workers are busy for 0/1 seconds
                    try {
                        Thread.sleep(rand.nextInt(2) * 1000);
                    }
                    catch (InterruptedException e) {
                    }

                    msg.send(worker);
                }
            }
        }
    }

    //  The main task begins by setting-up all its sockets. The local frontend
    //  talks to clients, and our local backend talks to workers. The cloud
    //  frontend talks to peer brokers as if they were clients, and the cloud
    //  backend talks to peer brokers as if they were workers. The state
    //  backend publishes regular state messages, and the state frontend
    //  subscribes to all state backends to collect these messages. Finally,
    //  we use a PULL monitor socket to collect printable messages from tasks:
    public static void main(String[] argv)
    {
        //  First argument is this broker's name
        //  Other arguments are our peers' names
        //
        if (argv.length < 1) {
            System.out.println("syntax: peering3 me {you}");
            System.exit(-1);
        }
        self = argv[0];
        System.out.printf("I: preparing broker at %s\n", self);
        Random rand = new Random(System.nanoTime());

        try (ZContext ctx = new ZContext()) {
            //  Prepare local frontend and backend
            Socket localfe = ctx.createSocket(SocketType.ROUTER);
            localfe.bind(String.format("ipc://%s-localfe.ipc", self));
            Socket localbe = ctx.createSocket(SocketType.ROUTER);
            localbe.bind(String.format("ipc://%s-localbe.ipc", self));

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

            //  Bind state backend to endpoint
            Socket statebe = ctx.createSocket(SocketType.PUB);
            statebe.bind(String.format("ipc://%s-state.ipc", self));

            //  Connect statefe to all peers
            Socket statefe = ctx.createSocket(SocketType.SUB);
            statefe.subscribe(ZMQ.SUBSCRIPTION_ALL);
            for (argn = 1; argn < argv.length; argn++) {
                String peer = argv[argn];
                System.out.printf(
                    "I: connecting to state backend at '%s'\n", peer
                );
                statefe.connect(String.format("ipc://%s-state.ipc", peer));
            }

            //  Prepare monitor socket
            Socket monitor = ctx.createSocket(SocketType.PULL);
            monitor.bind(String.format("ipc://%s-monitor.ipc", self));

            //  Start local workers
            int worker_nbr;
            for (worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++)
                new worker_task().start();

            //  Start local clients
            int client_nbr;
            for (client_nbr = 0; client_nbr < NBR_CLIENTS; client_nbr++)
                new client_task().start();

            //  Queue of available workers
            int localCapacity = 0;
            int cloudCapacity = 0;
            ArrayList<ZFrame> workers = new ArrayList<ZFrame>();

            //  The main loop has two parts. First we poll workers and our two
            //  service sockets (statefe and monitor), in any case. If we have
            //  no ready workers, there's no point in looking at incoming
            //  requests. These can remain on their internal 0MQ queues:
            Poller primary = ctx.createPoller(4);
            primary.register(localbe, Poller.POLLIN);
            primary.register(cloudbe, Poller.POLLIN);
            primary.register(statefe, Poller.POLLIN);
            primary.register(monitor, Poller.POLLIN);

            Poller secondary = ctx.createPoller(2);
            secondary.register(localfe, Poller.POLLIN);
            secondary.register(cloudfe, Poller.POLLIN);

            while (true) {
                //  First, route any waiting replies from workers

                //  If we have no workers anyhow, wait indefinitely
                int rc = primary.poll(localCapacity > 0 ? 1000 : -1);
                if (rc == -1)
                    break; //  Interrupted

                //  Track if capacity changes during this iteration
                int previous = localCapacity;

                //  Handle reply from local worker
                ZMsg msg = null;
                if (primary.pollin(0)) {
                    msg = ZMsg.recvMsg(localbe);
                    if (msg == null)
                        break; //  Interrupted
                    ZFrame address = msg.unwrap();
                    workers.add(address);
                    localCapacity++;

                    //  If it's READY, don't route the message any further
                    ZFrame frame = msg.getFirst();
                    String frameData = new String(frame.getData(), ZMQ.CHARSET);
                    if (frameData.equals(WORKER_READY)) {
                        msg.destroy();
                        msg = null;
                    }
                }
                //  Or handle reply from peer broker
                else if (primary.pollin(1)) {
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

                //  If we have input messages on our statefe or monitor sockets
                //  we can process these immediately:

                if (primary.pollin(2)) {
                    String peer = statefe.recvStr();
                    String status = statefe.recvStr();
                    cloudCapacity = Integer.parseInt(status);
                }
                if (primary.pollin(3)) {
                    String status = monitor.recvStr();
                    System.out.println(status);
                }

                //  Now we route as many client requests as we have worker
                //  capacity for. We may reroute requests from our local
                //  frontend, but not from the cloud frontend. We reroute
                //  randomly now, just to test things out. In the next version
                //  we'll do this properly by calculating cloud capacity.

                while (localCapacity + cloudCapacity > 0) {
                    rc = secondary.poll(0);

                    assert (rc >= 0);

                    if (secondary.pollin(0)) {
                        msg = ZMsg.recvMsg(localfe);
                    }
                    else if (localCapacity > 0 && secondary.pollin(1)) {
                        msg = ZMsg.recvMsg(cloudfe);
                    }
                    else break; //  No work, go back to backends

                    if (localCapacity > 0) {
                        ZFrame frame = workers.remove(0);
                        msg.wrap(frame);
                        msg.send(localbe);
                        localCapacity--;

                    }
                    else {
                        //  Route to random broker peer
                        int random_peer = rand.nextInt(argv.length - 1) + 1;
                        msg.push(argv[random_peer]);
                        msg.send(cloudbe);
                    }
                }

                //  We broadcast capacity messages to other peers; to reduce
                //  chatter we do this only if our capacity changed.
                if (localCapacity != previous) {
                    //  We stick our own address onto the envelope
                    statebe.sendMore(self);
                    //  Broadcast new capacity
                    statebe.send(String.format("%d", localCapacity), 0);
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
