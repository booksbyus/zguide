package guide;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMsg;

//  Binary Star server proof-of-concept implementation. This server does no
//  real work; it just demonstrates the Binary Star failover model.
public class bstarsrv
{
    //  States we can be in at any point in time
    enum State
    {
        STATE_PRIMARY, //  Primary, waiting for peer to connect
        STATE_BACKUP, //  Backup, waiting for peer to connect
        STATE_ACTIVE, //  Active - accepting connections
        STATE_PASSIVE //  Passive - not accepting connections
    }

    //  Events, which start with the states our peer can be in
    enum Event
    {
        PEER_PRIMARY, //  HA peer is pending primary
        PEER_BACKUP, //  HA peer is pending backup
        PEER_ACTIVE, //  HA peer is active
        PEER_PASSIVE, //  HA peer is passive
        CLIENT_REQUEST //  Client makes request
    }

    //  Our finite state machine
    private State state;      //  Current state
    private Event event;      //  Current event
    private long  peerExpiry; //  When peer is considered 'dead'

    //  We send state information this often
    //  If peer doesn't respond in two heartbeats, it is 'dead'
    private final static long HEARTBEAT = 1000; //  In msecs

    //  .split Binary Star state machine
    //  The heart of the Binary Star design is its finite-state machine (FSM).
    //  The FSM runs one event at a time. We apply an event to the current state,
    //  which checks if the event is accepted, and if so, sets a new state:

    private boolean stateMachine()
    {
        boolean exception = false;

        //  These are the PRIMARY and BACKUP states; we're waiting to become
        //  ACTIVE or PASSIVE depending on events we get from our peer:
        if (state == State.STATE_PRIMARY) {
            if (event == Event.PEER_BACKUP) {
                System.out.printf("I: connected to backup (passive), ready active\n");
                state = State.STATE_ACTIVE;
            }
            else if (event == Event.PEER_ACTIVE) {
                System.out.printf("I: connected to backup (active), ready passive\n");
                state = State.STATE_PASSIVE;
            }
            //  Accept client connections
        }
        else if (state == State.STATE_BACKUP) {
            if (event == Event.PEER_ACTIVE) {
                System.out.printf("I: connected to primary (active), ready passive\n");
                state = State.STATE_PASSIVE;
            }
            else
            //  Reject client connections when acting as backup
            if (event == Event.CLIENT_REQUEST)
                exception = true;
        }
        else
        //  .split active and passive states
        //  These are the ACTIVE and PASSIVE states:
        if (state == State.STATE_ACTIVE) {
            if (event == Event.PEER_ACTIVE) {
                //  Two actives would mean split-brain
                System.out.printf("E: fatal error - dual actives, aborting\n");
                exception = true;
            }
        }
        else
        //  Server is passive
        //  CLIENT_REQUEST events can trigger failover if peer looks dead
        if (state == State.STATE_PASSIVE) {
            if (event == Event.PEER_PRIMARY) {
                //  Peer is restarting - become active, peer will go passive
                System.out.printf("I: primary (passive) is restarting, ready active\n");
                state = State.STATE_ACTIVE;
            }
            else if (event == Event.PEER_BACKUP) {
                //  Peer is restarting - become active, peer will go passive
                System.out.printf("I: backup (passive) is restarting, ready active\n");
                state = State.STATE_ACTIVE;
            }
            else if (event == Event.PEER_PASSIVE) {
                //  Two passives would mean cluster would be non-responsive
                System.out.printf("E: fatal error - dual passives, aborting\n");
                exception = true;
            }
            else if (event == Event.CLIENT_REQUEST) {
                //  Peer becomes active if timeout has passed
                //  It's the client request that triggers the failover
                assert (peerExpiry > 0);
                if (System.currentTimeMillis() >= peerExpiry) {
                    //  If peer is dead, switch to the active state
                    System.out.printf("I: failover successful, ready active\n");
                    state = State.STATE_ACTIVE;
                }
                else
                    //  If peer is alive, reject connections
                    exception = true;
            }
        }
        return exception;
    }

    //  .split main task
    //  This is our main task. First we bind/connect our sockets with our
    //  peer and make sure we will get state messages correctly. We use
    //  three sockets; one to publish state, one to subscribe to state, and
    //  one for client requests/replies:

    public static void main(String[] argv)
    {
        //  Arguments can be either of:
        //      -p  primary server, at tcp://localhost:5001
        //      -b  backup server, at tcp://localhost:5002

        try (ZContext ctx = new ZContext()) {
            Socket statepub = ctx.createSocket(SocketType.PUB);
            Socket statesub = ctx.createSocket(SocketType.SUB);
            statesub.subscribe(ZMQ.SUBSCRIPTION_ALL);
            Socket frontend = ctx.createSocket(SocketType.ROUTER);
            bstarsrv fsm = new bstarsrv();

            if (argv.length == 1 && argv[0].equals("-p")) {
                System.out.printf("I: Primary active, waiting for backup (passive)\n");
                frontend.bind("tcp://*:5001");
                statepub.bind("tcp://*:5003");
                statesub.connect("tcp://localhost:5004");
                fsm.state = State.STATE_PRIMARY;
            }
            else if (argv.length == 1 && argv[0].equals("-b")) {
                System.out.printf("I: Backup passive, waiting for primary (active)\n");
                frontend.bind("tcp://*:5002");
                statepub.bind("tcp://*:5004");
                statesub.connect("tcp://localhost:5003");
                fsm.state = State.STATE_BACKUP;
            }
            else {
                System.out.printf("Usage: bstarsrv { -p | -b }\n");
                ctx.destroy();
                System.exit(0);
            }
            //  .split handling socket input
            //  We now process events on our two input sockets, and process
            //  these events one at a time via our finite-state machine. Our
            //  "work" for a client request is simply to echo it back.
            Poller poller = ctx.createPoller(2);
            poller.register(frontend, ZMQ.Poller.POLLIN);
            poller.register(statesub, ZMQ.Poller.POLLIN);

            //  Set timer for next outgoing state message
            long sendStateAt = System.currentTimeMillis() + HEARTBEAT;
            while (!Thread.currentThread().isInterrupted()) {
                int timeLeft = (int) ((sendStateAt - System.currentTimeMillis()));
                if (timeLeft < 0)
                    timeLeft = 0;
                int rc = poller.poll(timeLeft);
                if (rc == -1)
                    break; //  Context has been shut down

                if (poller.pollin(0)) {
                    //  Have a client request
                    ZMsg msg = ZMsg.recvMsg(frontend);
                    fsm.event = Event.CLIENT_REQUEST;
                    if (fsm.stateMachine() == false)
                        //  Answer client by echoing request back
                        msg.send(frontend);
                    else msg.destroy();
                }
                if (poller.pollin(1)) {
                    //  Have state from our peer, execute as event
                    String message = statesub.recvStr();
                    fsm.event = Event.values()[Integer.parseInt(message)];
                    if (fsm.stateMachine())
                        break; //  Error, so exit
                    fsm.peerExpiry = System.currentTimeMillis() + 2 * HEARTBEAT;
                }
                //  If we timed out, send state to peer
                if (System.currentTimeMillis() >= sendStateAt) {
                    statepub.send(String.valueOf(fsm.state.ordinal()));
                    sendStateAt = System.currentTimeMillis() + HEARTBEAT;
                }
            }

            if (Thread.currentThread().isInterrupted())
                System.out.printf("W: interrupted\n");
        }
    }
}
