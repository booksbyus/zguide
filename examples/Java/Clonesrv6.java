import org.zeromq.ZContext;
import org.zeromq.ZLoop;
import org.zeromq.ZLoop.IZLoopHandler;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.PollItem;
import org.zeromq.ZMQ.Socket;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

//  Clone server - Model Six
public class Clonesrv6
{
    private ZContext ctx;               //  Context wrapper
    private Map<String, Kvmsg> kvmap;   //  Key-value store
    private Bstar bStar;                //  Bstar reactor core
    private long sequence;              //  How many updates we're at
    private int port;                   //  Main port we're working on
    private int peer;                   //  Main port of our peer
    private Socket publisher;           //  Publish updates and hugz
    private Socket collector;           //  Collect updates from clients
    private Socket subscriber;          //  Get updates from peer
    private List<Kvmsg> pending;       //  Pending updates from clients
    private boolean primary;            //  TRUE if we're primary
    private boolean active;             //  TRUE if we're active
    private boolean passive;            //  TRUE if we're passive

    private static class Snapshots implements IZLoopHandler
    {
        @Override
        public int handle(ZLoop loop, PollItem item, Object arg)
        {
            Clonesrv6 srv = (Clonesrv6) arg;
            Socket socket = item.getSocket();

            byte[] identity = socket.recv();
            if (identity != null) {
                //  Request is in second frame of message
                String request = socket.recvStr();
                String subtree = null;
                if (request.equals("ICANHAZ?")) {
                    subtree = socket.recvStr();
                }
                else
                    System.out.printf("E: bad request, aborting\n");

                if (subtree != null) {
                    //  Send state socket to client
                    for (Entry<String, Kvmsg> entry: srv.kvmap.entrySet()) {
                        sendSingle(entry.getValue(), identity, subtree, socket);
                    }

                    //  Now send END message with getSequence number
                    System.out.printf("I: sending shapshot=%d\n", srv.sequence);
                    socket.send(identity, ZMQ.SNDMORE);
                    Kvmsg kvmsg = new Kvmsg(srv.sequence);
                    kvmsg.setKey("KTHXBAI");
                    kvmsg.setBody(subtree.getBytes());
                    kvmsg.send(socket);
                    kvmsg.destroy();
                }
            }
            return 0;
        }
    }

    private static class Collector implements IZLoopHandler
    {
        @Override
        public int handle(ZLoop loop, PollItem item, Object arg)
        {
            Clonesrv6 srv = (Clonesrv6) arg;
            Socket socket = item.getSocket();

            Kvmsg msg = Kvmsg.recv(socket);
            if (msg != null) {
                msg.setSequence(++srv.sequence);
                msg.send(srv.publisher);
                int ttl = Integer.parseInt(msg.getProp("ttl"));
                if (ttl > 0)
                    msg.setProp("ttl",
                            "%d", System.currentTimeMillis() + ttl * 1000);
                msg.store(srv.kvmap);
                System.out.printf("I: publishing update=%d\n", srv.sequence);
            } else {
                //  If we already got message from active, drop it, else
                //  hold on pending list
                if (srv.wasPending(msg))
                    msg.destroy();
                else
                    srv.pending.add(msg);
            }


            return 0;
        }
    }

    //  .split heartbeating
    //  We send a HUGZ message once a second to all subscribers so that they
    //  can detect if our server dies. They'll then switch over to the backup
    //  server, which will become active:
    private static class SendHugz implements IZLoopHandler
    {
        @Override
        public int handle(ZLoop loop, PollItem item, Object arg)
        {
            Clonesrv6 srv = (Clonesrv6) arg;

            Kvmsg msg = new Kvmsg(srv.sequence);
            msg.setKey("HUGZ");
            msg.setBody("".getBytes());
            msg.send(srv.publisher);
            msg.destroy();

            return 0;
        }
    }

    private static class FlushTTL implements IZLoopHandler
    {
        @Override
        public int handle(ZLoop loop, PollItem item, Object arg)
        {
            Clonesrv6 srv = (Clonesrv6) arg;
            if (srv.kvmap != null) {
                for (Kvmsg msg: new ArrayList<Kvmsg>(srv.kvmap.values())) {
                    srv.flushSingle(msg);
                }
            }
            return 0;
        }
    }

    //  .split handling state changes
    //  When we switch from passive to active, we apply our pending list so that
    //  our kvmap is up-to-date. When we switch to passive, we wipe our kvmap
    //  and grab a new snapshot from the active server:
    private static class NewActive implements IZLoopHandler
    {
        @Override
        public int handle(ZLoop loop, PollItem item, Object arg)
        {
            Clonesrv6 srv = (Clonesrv6) arg;

            srv.active = true;
            srv.passive = false;

            //  Stop subscribing to updates
            PollItem poller = new PollItem(srv.subscriber, ZMQ.Poller.POLLIN);
            srv.bStar.zloop().removePoller(poller);

            //  Apply pending list to own hash table
            for (Kvmsg msg: srv.pending) {
                msg.setSequence(++srv.sequence);
                msg.send(srv.publisher);
                msg.store(srv.kvmap);
                System.out.printf("I: publishing pending=%d", srv.sequence);
            }

            return 0;
        }
    }

    private static class NewPassive implements IZLoopHandler
    {
        @Override
        public int handle(ZLoop loop, PollItem item, Object arg)
        {
            Clonesrv6 srv = (Clonesrv6) arg;

            if (srv.kvmap != null) {
                for (Kvmsg msg: srv.kvmap.values())
                    msg.destroy();
            }
            srv.active = false;
            srv.passive = true;

            //  Start subscribing to updates
            PollItem poller = new PollItem(srv.subscriber, ZMQ.Poller.POLLIN);
            srv.bStar.zloop().addPoller(poller, new Subscriber(), srv);
            return 0;
        }
    }

    //  .split subscriber handler
    //  When we get an update, we create a new kvmap if necessary, and then
    //  add our update to our kvmap. We're always passive in this case:
    private static class Subscriber implements IZLoopHandler
    {
        @Override
        public int handle(ZLoop loop, PollItem item, Object arg)
        {
            Clonesrv6 srv = (Clonesrv6) arg;
            Socket socket = item.getSocket();

            //  Get state snapshot if necessary
            if (srv.kvmap == null) {
                srv.kvmap = new HashMap<String, Kvmsg>();
                Socket snapshot = srv.ctx.createSocket(ZMQ.DEALER);
                snapshot.connect(String.format("tcp://localhost:%d", srv.peer));

                System.out.printf("I: asking for snapshot from: tcp://localhost:%d",
                        srv.peer);
                snapshot.sendMore("ICANHAZ?");
                snapshot.send(""); // blank subtree to get all

                while (true) {
                    Kvmsg msg = Kvmsg.recv(snapshot);
                    if (msg == null)
                        break;          //  Interrupted
                    if (msg.getKey().equals("KTHXBAI")) {
                        srv.sequence = msg.getSequence();
                        msg.destroy();
                        break;          //  Done
                    }
                    msg.store(srv.kvmap);
                }
                System.out.printf("I: received snapshot=%d",srv.sequence);
                srv.ctx.destroySocket(snapshot);

            }

            //  Find and remove update off pending list
            Kvmsg msg = Kvmsg.recv(item.getSocket());
            if (msg == null)
                return 0;

            if (msg.getKey().equals("HUGZ")) {
                if (!srv.wasPending(msg)) {
                    //  If active update came before client update, flip it
                    //  around, store active update (with sequence) on pending
                    //  list and use to clear client update when it comes later
                    srv.pending.add(msg.dup());
                }
                //  If update is more recent than our kvmap, apply it
                if (msg.getSequence() > srv.sequence) {
                    srv.sequence = msg.getSequence();
                    msg.store(srv.kvmap);
                    System.out.printf("I: received update=%d",srv.sequence);
                }
            }
            msg.destroy();

            return 0;
        }
    }

    public Clonesrv6(boolean primary)
    {
        if (primary) {
            bStar = new Bstar(true, "tcp://*:5003",
                "tcp://localhost:5004");
            bStar.voter("tcp://*:5556",
                ZMQ.ROUTER, new Snapshots(), this);

            port = 5556;
            peer = 5566;
            this.primary = true;
        } else {
            bStar = new Bstar(false, "tcp://*:5004",
                    "tcp://localhost:5003");
            bStar.voter("tcp://*:5566",
                    ZMQ.ROUTER, new Snapshots(), this);

            port = 5566;
            peer = 5556;
            this.primary = false;
        }

        //  Primary server will become first active
        if (primary)
            kvmap = new HashMap<String, Kvmsg>();

        ctx = new ZContext();
        pending = new ArrayList<Kvmsg>();
        bStar.setVerbose(true);

        //  Set up our Clone server sockets
        publisher = ctx.createSocket(ZMQ.PUB);
        collector = ctx.createSocket(ZMQ.SUB);
        collector.subscribe("".getBytes());
        publisher.bind(String.format("tcp://*:%d", port + 1));
        collector.bind(String.format("tcp://*:%d", port+2));

        //  Set up our own Clone client interface to peer
        subscriber = ctx.createSocket(ZMQ.SUB);
        subscriber.subscribe("".getBytes());
        subscriber.connect(String.format("tcp://localhost:%d", peer + 1));
    }

    //  .split main task body
    //  After we've setup our sockets, we register our binary star
    //  event handlers, and then start the Bstar reactor. This finishes
    //  when the user presses Ctrl-C or when the process receives a SIGINT
    //  Interrupt:
    public void run()
    {
        //  Register state change handlers
        bStar.newActive(new NewActive(), this);
        bStar.newPassive(new NewPassive(), this);

        //  Register our other handlers with the Bstar reactor
        PollItem poller = new PollItem(collector, ZMQ.Poller.POLLIN);

        bStar.zloop().addPoller(poller, new Collector(), this);
        bStar.zloop().addTimer(1000, 0, new FlushTTL(), this);
        bStar.zloop().addTimer(1000, 0, new SendHugz(), this);

        //  Start the Bstar reactor
        bStar.start();

        //  Interrupted, so shut down
        for (Kvmsg value: pending)
            value.destroy();

        bStar.destroy();
        for (Kvmsg value: kvmap.values())
            value.destroy();

        ctx.destroy();
    }

    //  Send one state snapshot key-value pair to a socket
    //  Hash item data is our Kvmsg object, ready to send
    private static void sendSingle(Kvmsg msg, byte[] identity, String subtree, Socket socket)
    {
        if (msg.getKey().startsWith(subtree)) {
            socket.send (identity,    //  Choose recipient
                            ZMQ.SNDMORE);
            msg.send(socket);
        }
    }

    //  The collector is more complex than in the Clonesrv5 example because the
    //  way it processes updates depends on whether we're active or passive.
    //  The active applies them immediately to its kvmap, whereas the passive
    //  queues them as pending:

    //  If message was already on pending list, remove it and return TRUE,
    //  else return FALSE.
    boolean wasPending (Kvmsg msg)
    {
        Iterator<Kvmsg> it = pending.iterator();
        while (it.hasNext()) {
            if (msg.UUID().equals(it.next().UUID())) {
                it.remove();
                return true;
            }

        }
        return false;
    }


    //  We purge ephemeral values using exactly the same code as in
    //  the previous Clonesrv5 example.
    //  .skip
    //  If key-value pair has expired, delete it and publish the
    //  fact to listening clients.
    private void flushSingle(Kvmsg msg)
    {
        long ttl = Long.parseLong(msg.getProp("ttl"));
        if (ttl > 0 && System.currentTimeMillis() >= ttl) {
            msg.setSequence(++sequence);
            msg.setBody("".getBytes());
            msg.send(publisher);
            msg.store(kvmap);
            System.out.printf("I: publishing delete=%d\n", sequence);
        }
    }

    //  .split main task setup
    //  The main task parses the command line to decide whether to start
    //  as a primary or backup server. We're using the Binary Star pattern
    //  for reliability. This interconnects the two servers so they can
    //  agree on which one is primary and which one is backup. To allow the
    //  two servers to run on the same box, we use different ports for
    //  primary and backup. Ports 5003/5004 are used to interconnect the
    //  servers. Ports 5556/5566 are used to receive voting events (snapshot
    //  requests in the Clone pattern). Ports 5557/5567 are used by the
    //  publisher, and ports 5558/5568 are used by the collector:
    public static void main(String[] args)
    {
        Clonesrv6 srv = null;

        if (args.length == 1 && "-p".equals(args[0])) {
            srv = new Clonesrv6(true);
        } else if (args.length == 1 && "-b".equals(args[0])) {
            srv = new Clonesrv6(false);
        } else {
            System.out.printf("Usage: Clonesrv4 { -p | -b }\n");
            System.exit(0);
        }
        srv.run();
    }
}
