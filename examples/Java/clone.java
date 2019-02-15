package guide;

import java.util.HashMap;
import java.util.Map;

import org.zeromq.*;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZThread.IAttachedRunnable;

public class clone
{
    private ZContext ctx;  //  Our context wrapper
    private Socket   pipe; //  Pipe through to clone agent

    //  .split constructor and destructor
    //  Here are the constructor and destructor for the clone class. Note that
    //  we create a context specifically for the pipe that connects our
    //  frontend to the backend agent:
    public clone()
    {
        ctx = new ZContext();
        pipe = ZThread.fork(ctx, new CloneAgent());
    }

    public void destroy()
    {
        ctx.destroy();
    }

    //  .split subtree method
    //  Specify subtree for snapshot and updates, which we must do before
    //  connecting to a server as the subtree specification is sent as the
    //  first command to the server. Sends a [SUBTREE][subtree] command to
    //  the agent:
    public void subtree(String subtree)
    {
        ZMsg msg = new ZMsg();
        msg.add("SUBTREE");
        msg.add(subtree);
        msg.send(pipe);
    }

    //  .split connect method
    //  Connect to a new server endpoint. We can connect to at most two
    //  servers. Sends [CONNECT][endpoint][service] to the agent:
    public void connect(String address, String service)
    {
        ZMsg msg = new ZMsg();
        msg.add("CONNECT");
        msg.add(address);
        msg.add(service);
        msg.send(pipe);
    }

    //  .split set method
    //  Set a new value in the shared hashmap. Sends a [SET][key][value][ttl]
    //  command through to the agent which does the actual work:
    public void set(String key, String value, int ttl)
    {
        ZMsg msg = new ZMsg();
        msg.add("SET");
        msg.add(key);
        msg.add(value);
        msg.add(String.format("%d", ttl));
        msg.send(pipe);
    }

    //  .split get method
    //  Look up value in distributed hash table. Sends [GET][key] to the agent and
    //  waits for a value response. If there is no value available, will eventually
    //  return NULL:
    public String get(String key)
    {
        ZMsg msg = new ZMsg();
        msg.add("GET");
        msg.add(key);
        msg.send(pipe);

        ZMsg reply = ZMsg.recvMsg(pipe);
        if (reply != null) {
            String value = reply.popString();
            reply.destroy();
            return value;
        }
        return null;
    }

    //  .split working with servers
    //  The backend agent manages a set of servers, which we implement using
    //  our simple class model:
    private static class Server
    {
        private String address;    //  Server address
        private int    port;       //  Server port
        private Socket snapshot;   //  Snapshot socket
        private Socket subscriber; //  Incoming updates
        private long   expiry;     //  When server expires
        private int    requests;   //  How many snapshot requests made?

        protected Server(ZContext ctx, String address, int port, String subtree)
        {
            System.out.printf("I: adding server %s:%d...\n", address, port);
            this.address = address;
            this.port = port;

            snapshot = ctx.createSocket(SocketType.DEALER);
            snapshot.connect(String.format("%s:%d", address, port));
            subscriber = ctx.createSocket(SocketType.SUB);
            subscriber.connect(String.format("%s:%d", address, port + 1));
            subscriber.subscribe(subtree.getBytes(ZMQ.CHARSET));
        }

        protected void destroy()
        {
        }
    }

    //  .split backend agent class
    //  Here is the implementation of the backend agent itself:

    //  Number of servers to which we will talk to
    private final static int SERVER_MAX = 2;

    //  Server considered dead if silent for this long
    private final static int SERVER_TTL = 5000; //  msecs

    //  States we can be in
    private final static int STATE_INITIAL = 0; //  Before asking server for state
    private final static int STATE_SYNCING = 1; //  Getting state from server
    private final static int STATE_ACTIVE  = 2; //  Getting new updates from server

    private static class Agent
    {
        private ZContext            ctx;        //  Context wrapper
        private Socket              pipe;       //  Pipe back to application
        private Map<String, String> kvmap;      //  Actual key/value table
        private String              subtree;    //  Subtree specification, if any
        private Server[]            server;
        private int                 nbrServers; //  0 to SERVER_MAX
        private int                 state;      //  Current state
        private int                 curServer;  //  If active, server 0 or 1
        private long                sequence;   //  Last kvmsg processed
        private Socket              publisher;  //  Outgoing updates

        protected Agent(ZContext ctx, Socket pipe)
        {
            this.ctx = ctx;
            this.pipe = pipe;
            kvmap = new HashMap<String, String>();
            subtree = "";
            state = STATE_INITIAL;
            publisher = ctx.createSocket(SocketType.PUB);

            server = new Server[SERVER_MAX];
        }

        protected void destroy()
        {
            for (int serverNbr = 0; serverNbr < nbrServers; serverNbr++)
                server[serverNbr].destroy();
        }

        //  .split handling a control message
        //  Here we handle the different control messages from the frontend;
        //  SUBTREE, CONNECT, SET, and GET:
        private boolean controlMessage()
        {
            ZMsg msg = ZMsg.recvMsg(pipe);
            String command = msg.popString();
            if (command == null)
                return false; //  Interrupted

            if (command.equals("SUBTREE")) {
                subtree = msg.popString();
            }
            else if (command.equals("CONNECT")) {
                String address = msg.popString();
                String service = msg.popString();
                if (nbrServers < SERVER_MAX) {
                    server[nbrServers++] = new Server(ctx, address, Integer.parseInt(service), subtree);
                    //  We broadcast updates to all known servers
                    publisher.connect(String.format("%s:%d", address, Integer.parseInt(service) + 2));
                }
                else System.out.printf("E: too many servers (max. %d)\n", SERVER_MAX);
            }
            else
            //  .split set and get commands
            //  When we set a property, we push the new key-value pair onto
            //  all our connected servers:
            if (command.equals("SET")) {
                String key = msg.popString();
                String value = msg.popString();
                String ttl = msg.popString();
                kvmap.put(key, value);

                //  Send key-value pair on to server
                kvmsg kvmsg = new kvmsg(0);
                kvmsg.setKey(key);
                kvmsg.setUUID();
                kvmsg.fmtBody("%s", value);
                kvmsg.setProp("ttl", ttl);
                kvmsg.send(publisher);
                kvmsg.destroy();
            }
            else if (command.equals("GET")) {
                String key = msg.popString();
                String value = kvmap.get(key);
                if (value != null)
                    pipe.send(value);
                else pipe.send("");
            }
            msg.destroy();

            return true;
        }
    }

    private static class CloneAgent implements IAttachedRunnable
    {

        @Override
        public void run(Object[] args, ZContext ctx, Socket pipe)
        {
            Agent self = new Agent(ctx, pipe);

            Poller poller = ctx.createPoller(1);
            poller.register(pipe, Poller.POLLIN);

            while (!Thread.currentThread().isInterrupted()) {
                long pollTimer = -1;
                int pollSize = 2;
                Server server = self.server[self.curServer];
                switch (self.state) {
                case STATE_INITIAL:
                    //  In this state we ask the server for a snapshot,
                    //  if we have a server to talk to...
                    if (self.nbrServers > 0) {
                        System.out.printf("I: waiting for server at %s:%d...\n", server.address, server.port);
                        if (server.requests < 2) {
                            server.snapshot.sendMore("ICANHAZ?");
                            server.snapshot.send(self.subtree);
                            server.requests++;
                        }
                        server.expiry = System.currentTimeMillis() + SERVER_TTL;
                        self.state = STATE_SYNCING;

                        poller.close();
                        poller = ctx.createPoller(2);
                        poller.register(pipe, Poller.POLLIN);
                        poller.register(server.snapshot, Poller.POLLIN);
                    }
                    else pollSize = 1;
                    break;

                case STATE_SYNCING:
                    //  In this state we read from snapshot and we expect
                    //  the server to respond, else we fail over.
                    poller.close();
                    poller = ctx.createPoller(2);
                    poller.register(pipe, Poller.POLLIN);
                    poller.register(server.snapshot, Poller.POLLIN);
                    break;

                case STATE_ACTIVE:
                    //  In this state we read from subscriber and we expect
                    //  the server to give hugz, else we fail over.
                    poller.close();
                    poller = ctx.createPoller(2);
                    poller.register(pipe, Poller.POLLIN);
                    poller.register(server.subscriber, Poller.POLLIN);
                    break;
                }
                if (server != null) {
                    pollTimer = server.expiry - System.currentTimeMillis();
                    if (pollTimer < 0)
                        pollTimer = 0;
                }
                //  .split client poll loop
                //  We're ready to process incoming messages; if nothing at all
                //  comes from our server within the timeout, that means the
                //  server is dead:
                int rc = poller.poll(pollTimer);
                if (rc == -1)
                    break; //  Context has been shut down

                if (poller.pollin(0)) {
                    if (!self.controlMessage())
                        break; //  Interrupted
                }
                else if (pollSize == 2 && poller.pollin(1)) {
                    kvmsg msg = kvmsg.recv(poller.getSocket(1));
                    if (msg == null)
                        break; //  Interrupted

                    //  Anything from server resets its expiry time
                    server.expiry = System.currentTimeMillis() + SERVER_TTL;
                    if (self.state == STATE_SYNCING) {
                        //  Store in snapshot until we're finished
                        server.requests = 0;
                        if (msg.getKey().equals("KTHXBAI")) {
                            self.sequence = msg.getSequence();
                            self.state = STATE_ACTIVE;
                            System.out.printf("I: received from %s:%d snapshot=%d\n", server.address, server.port,
                                    self.sequence);
                            msg.destroy();
                        }
                    }
                    else if (self.state == STATE_ACTIVE) {
                        //  Discard out-of-sequence updates, incl. hugz
                        if (msg.getSequence() > self.sequence) {
                            self.sequence = msg.getSequence();
                            System.out.printf("I: received from %s:%d update=%d\n", server.address, server.port,
                                    self.sequence);
                        }
                        else msg.destroy();
                    }
                }
                else {
                    //  Server has died, failover to next
                    System.out.printf("I: server at %s:%d didn't give hugz\n", server.address, server.port);
                    self.curServer = (self.curServer + 1) % self.nbrServers;
                    self.state = STATE_INITIAL;
                }
            }
            self.destroy();
        }
    }

}
