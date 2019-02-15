package guide;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.zeromq.*;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZThread.IAttachedRunnable;

//  flcliapi class - Freelance Pattern agent class
//  Implements the Freelance Protocol at http://rfc.zeromq.org/spec:10
public class flcliapi
{
    //  If not a single service replies within this time, give up
    private static final int GLOBAL_TIMEOUT = 2500;
    //  PING interval for servers we think are alive
    private static final int PING_INTERVAL = 2000; //  msecs
    //  Server considered dead if silent for this long
    private static final int SERVER_TTL = 6000; //  msecs

    //  .split API structure
    //  This API works in two halves, a common pattern for APIs that need to
    //  run in the background. One half is an frontend object our application
    //  creates and works with; the other half is a backend "agent" that runs
    //  in a background thread. The frontend talks to the backend over an
    //  inproc pipe socket:

    //  Structure of our frontend class
    private ZContext ctx;  //  Our context wrapper
    private Socket   pipe; //  Pipe through to flcliapi agent

    public flcliapi()
    {
        ctx = new ZContext();
        FreelanceAgent agent = new FreelanceAgent();
        pipe = ZThread.fork(ctx, agent);
    }

    public void destroy()
    {
        ctx.destroy();
    }

    //  .split connect method
    //  To implement the connect method, the frontend object sends a multipart
    //  message to the backend agent. The first part is a string "CONNECT", and
    //  the second part is the endpoint. It waits 100msec for the connection to
    //  come up, which isn't pretty, but saves us from sending all requests to a
    //  single server, at startup time:
    public void connect(String endpoint)
    {
        ZMsg msg = new ZMsg();
        msg.add("CONNECT");
        msg.add(endpoint);

        msg.send(pipe);
        try {
            Thread.sleep(100); //  Allow connection to come up
        }
        catch (InterruptedException e) {
        }
    }

    //  .split request method
    //  To implement the request method, the frontend object sends a message
    //  to the backend, specifying a command "REQUEST" and the request message:
    public ZMsg request(ZMsg request)
    {
        request.push("REQUEST");
        request.send(pipe);
        ZMsg reply = ZMsg.recvMsg(pipe);
        if (reply != null) {
            String status = reply.popString();
            if (status.equals("FAILED"))
                reply.destroy();
        }
        return reply;
    }

    //  .split backend agent
    //  Here we see the backend agent. It runs as an attached thread, talking
    //  to its parent over a pipe socket. It is a fairly complex piece of work
    //  so we'll break it down into pieces. First, the agent manages a set of
    //  servers, using our familiar class approach:

    //  Simple class for one server we talk to
    private static class Server
    {
        private String  endpoint; //  Server identity/endpoint
        private boolean alive;    //  1 if known to be alive
        private long    pingAt;   //  Next ping at this time
        private long    expires;  //  Expires at this time

        protected Server(String endpoint)
        {
            this.endpoint = endpoint;
            alive = false;
            pingAt = System.currentTimeMillis() + PING_INTERVAL;
            expires = System.currentTimeMillis() + SERVER_TTL;
        }

        protected void destroy()
        {
        }

        private void ping(Socket socket)
        {
            if (System.currentTimeMillis() >= pingAt) {
                ZMsg ping = new ZMsg();
                ping.add(endpoint);
                ping.add("PING");
                ping.send(socket);
                pingAt = System.currentTimeMillis() + PING_INTERVAL;
            }
        }

        private long tickless(long tickless)
        {
            if (tickless > pingAt)
                return pingAt;
            return -1;
        }
    }

    //  .split backend agent class
    //  We build the agent as a class that's capable of processing messages
    //  coming in from its various sockets:

    //  Simple class for one background agent
    private static class Agent
    {
        private ZContext            ctx;      //  Own context
        private Socket              pipe;     //  Socket to talk back to application
        private Socket              router;   //  Socket to talk to servers
        private Map<String, Server> servers;  //  Servers we've connected to
        private List<Server>        actives;  //  Servers we know are alive
        private int                 sequence; //  Number of requests ever sent
        private ZMsg                request;  //  Current request if any
        private ZMsg                reply;    //  Current reply if any
        private long                expires;  //  Timeout for request/reply

        protected Agent(ZContext ctx, Socket pipe)
        {
            this.ctx = ctx;
            this.pipe = pipe;
            router = ctx.createSocket(SocketType.ROUTER);
            servers = new HashMap<String, Server>();
            actives = new ArrayList<Server>();
        }

        protected void destroy()
        {
            for (Server server : servers.values())
                server.destroy();
        }

        //  .split control messages
        //  This method processes one message from our frontend class
        //  (it's going to be CONNECT or REQUEST):

        //  Callback when we remove server from agent 'servers' hash table
        private void controlMessage()
        {
            ZMsg msg = ZMsg.recvMsg(pipe);
            String command = msg.popString();

            if (command.equals("CONNECT")) {
                String endpoint = msg.popString();
                System.out.printf("I: connecting to %s...\n", endpoint);
                router.connect(endpoint);
                Server server = new Server(endpoint);
                servers.put(endpoint, server);
                actives.add(server);
                server.pingAt = System.currentTimeMillis() + PING_INTERVAL;
                server.expires = System.currentTimeMillis() + SERVER_TTL;
            }
            else if (command.equals("REQUEST")) {
                assert (request == null); //  Strict request-reply cycle
                //  Prefix request with getSequence number and empty envelope
                String sequenceText = String.format("%d", ++sequence);
                msg.push(sequenceText);
                //  Take ownership of request message
                request = msg;
                msg = null;
                //  Request expires after global timeout
                expires = System.currentTimeMillis() + GLOBAL_TIMEOUT;
            }
            if (msg != null)
                msg.destroy();
        }

        //  .split router messages
        //  This method processes one message from a connected
        //  server:
        private void routerMessage()
        {
            ZMsg reply = ZMsg.recvMsg(router);

            //  Frame 0 is server that replied
            String endpoint = reply.popString();
            Server server = servers.get(endpoint);
            assert (server != null);
            if (!server.alive) {
                actives.add(server);
                server.alive = true;
            }
            server.pingAt = System.currentTimeMillis() + PING_INTERVAL;
            server.expires = System.currentTimeMillis() + SERVER_TTL;

            //  Frame 1 may be getSequence number for reply
            String sequenceStr = reply.popString();
            if (Integer.parseInt(sequenceStr) == sequence) {
                reply.push("OK");
                reply.send(pipe);
                request.destroy();
                request = null;
            }
            else reply.destroy();

        }

    }

    //  .split backend agent implementation
    //  Finally, here's the agent task itself, which polls its two sockets
    //  and processes incoming messages:
    static private class FreelanceAgent implements IAttachedRunnable
    {

        @Override
        public void run(Object[] args, ZContext ctx, Socket pipe)
        {
            Agent agent = new Agent(ctx, pipe);

            Poller poller = ctx.createPoller(2);
            poller.register(agent.pipe, Poller.POLLIN);
            poller.register(agent.router, Poller.POLLIN);

            while (!Thread.currentThread().isInterrupted()) {
                //  Calculate tickless timer, up to 1 hour
                long tickless = System.currentTimeMillis() + 1000 * 3600;
                if (agent.request != null && tickless > agent.expires)
                    tickless = agent.expires;

                for (Server server : agent.servers.values()) {
                    long newTickless = server.tickless(tickless);
                    if (newTickless > 0)
                        tickless = newTickless;
                }

                int rc = poller.poll(tickless - System.currentTimeMillis());
                if (rc == -1)
                    break; //  Context has been shut down

                if (poller.pollin(0))
                    agent.controlMessage();

                if (poller.pollin(1))
                    agent.routerMessage();

                //  If we're processing a request, dispatch to next server
                if (agent.request != null) {
                    if (System.currentTimeMillis() >= agent.expires) {
                        //  Request expired, kill it
                        agent.pipe.send("FAILED");
                        agent.request.destroy();
                        agent.request = null;
                    }
                    else {
                        //  Find server to talk to, remove any expired ones
                        while (!agent.actives.isEmpty()) {
                            Server server = agent.actives.get(0);
                            if (System.currentTimeMillis() >= server.expires) {
                                agent.actives.remove(0);
                                server.alive = false;
                            }
                            else {
                                ZMsg request = agent.request.duplicate();
                                request.push(server.endpoint);
                                request.send(agent.router);
                                break;
                            }
                        }
                    }
                }

                //  Disconnect and delete any expired servers
                //  Send heartbeats to idle servers if needed
                for (Server server : agent.servers.values())
                    server.ping(agent.router);
            }
            agent.destroy();
        }
    }
}
