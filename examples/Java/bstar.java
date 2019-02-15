package guide;

import org.zeromq.*;
import org.zeromq.ZLoop.IZLoopHandler;
import org.zeromq.ZMQ.PollItem;
import org.zeromq.ZMQ.Socket;

//  bstar class - Binary Star reactor
public class bstar
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

    private ZContext            ctx;        //  Our private context
    private ZLoop               loop;       //  Reactor loop
    private Socket              statepub;   //  State publisher
    private Socket              statesub;   //  State subscriber
    private State               state;      //  Current state
    private Event               event;      //  Current event
    private long                peerExpiry; //  When peer is considered 'dead'
    private ZLoop.IZLoopHandler voterFn;    //  Voting socket handler
    private Object              voterArg;   //  Arguments for voting handler
    private ZLoop.IZLoopHandler activeFn;   //  Call when become active
    private Object              activeArg;  //  Arguments for handler
    private ZLoop.IZLoopHandler passiveFn;  //  Call when become passive
    private Object              passiveArg; //  Arguments for handler

    //  The finite-state machine is the same as in the proof-of-concept server.
    //  To understand this reactor in detail, first read the ZLoop class.
    //  .skip

    //  We send state information this often
    //  If peer doesn't respond in two heartbeats, it is 'dead'
    private final static int BSTAR_HEARTBEAT = 1000; //  In msecs

    //  Binary Star finite state machine (applies event to state)
    //  Returns false if there was an exception, true if event was valid.

    private boolean execute()
    {
        boolean rc = true;

        //  Primary server is waiting for peer to connect
        //  Accepts CLIENT_REQUEST events in this state
        if (state == State.STATE_PRIMARY) {
            if (event == Event.PEER_BACKUP) {
                System.out.printf("I: connected to backup (passive), ready active\n");
                state = State.STATE_ACTIVE;
                if (activeFn != null)
                    activeFn.handle(loop, null, activeArg);
            }
            else if (event == Event.PEER_ACTIVE) {
                System.out.printf("I: connected to backup (active), ready passive\n");
                state = State.STATE_PASSIVE;
                if (passiveFn != null)
                    passiveFn.handle(loop, null, passiveArg);
            }
            else if (event == Event.CLIENT_REQUEST) {
                // Allow client requests to turn us into the active if we've
                // waited sufficiently long to believe the backup is not
                // currently acting as active (i.e., after a failover)
                assert (peerExpiry > 0);
                if (System.currentTimeMillis() >= peerExpiry) {
                    System.out.printf("I: request from client, ready as active\n");
                    state = State.STATE_ACTIVE;
                    if (activeFn != null)
                        activeFn.handle(loop, null, activeArg);
                }
                else
                    // Don't respond to clients yet - it's possible we're
                    // performing a failback and the backup is currently active
                    rc = false;
            }
        }
        else if (state == State.STATE_BACKUP) {
            if (event == Event.PEER_ACTIVE) {
                System.out.printf("I: connected to primary (active), ready passive\n");
                state = State.STATE_PASSIVE;
                if (passiveFn != null)
                    passiveFn.handle(loop, null, passiveArg);
            }
            else
            //  Reject client connections when acting as backup
            if (event == Event.CLIENT_REQUEST)
                rc = false;
        }
        else
        //  .split active and passive states
        //  These are the ACTIVE and PASSIVE states:
        if (state == State.STATE_ACTIVE) {
            if (event == Event.PEER_ACTIVE) {
                //  Two actives would mean split-brain
                System.out.printf("E: fatal error - dual actives, aborting\n");
                rc = false;
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
                rc = false;
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
                    rc = false;

                //  Call state change handler if necessary
                if (state == State.STATE_ACTIVE && activeFn != null)
                    activeFn.handle(loop, null, activeArg);
            }
        }
        return rc;
    }

    private void updatePeerExpiry()
    {
        peerExpiry = System.currentTimeMillis() + 2 * BSTAR_HEARTBEAT;
    }

    //  Reactor event handlers...

    //  Publish our state to peer
    private static IZLoopHandler SendState = new IZLoopHandler()
    {

        @Override
        public int handle(ZLoop loop, PollItem item, Object arg)
        {
            bstar self = (bstar) arg;
            self.statepub.send(String.format("%d", self.state.ordinal()));
            return 0;
        }
    };

    //  Receive state from peer, execute finite state machine
    private static IZLoopHandler RecvState = new IZLoopHandler()
    {

        @Override
        public int handle(ZLoop loop, PollItem item, Object arg)
        {
            bstar self = (bstar) arg;
            String state = item.getSocket().recvStr();
            if (state != null) {
                self.event = Event.values()[Integer.parseInt(state)];
                self.updatePeerExpiry();
            }
            return self.execute() ? 0 : -1;
        }
    };

    //  Application wants to speak to us, see if it's possible
    private static IZLoopHandler VoterReady = new IZLoopHandler()
    {

        @Override
        public int handle(ZLoop loop, PollItem item, Object arg)
        {
            bstar self = (bstar) arg;
            //  If server can accept input now, call appl handler
            self.event = Event.CLIENT_REQUEST;
            if (self.execute())
                self.voterFn.handle(loop, item, self.voterArg);
            else {
                //  Destroy waiting message, no-one to read it
                ZMsg msg = ZMsg.recvMsg(item.getSocket());
                msg.destroy();
            }
            return 0;
        }
    };

    //  .until
    //  .split constructor
    //  This is the constructor for our {{bstar}} class. We have to tell it
    //  whether we're primary or backup server, as well as our local and
    //  remote endpoints to bind and connect to:
    public bstar(boolean primary, String local, String remote)
    {
        //  Initialize the Binary Star
        ctx = new ZContext();
        loop = new ZLoop(ctx);
        state = primary ? State.STATE_PRIMARY : State.STATE_BACKUP;

        //  Create publisher for state going to peer
        statepub = ctx.createSocket(SocketType.PUB);
        statepub.bind(local);

        //  Create subscriber for state coming from peer
        statesub = ctx.createSocket(SocketType.SUB);
        statesub.subscribe(ZMQ.SUBSCRIPTION_ALL);
        statesub.connect(remote);

        //  Set-up basic reactor events
        loop.addTimer(BSTAR_HEARTBEAT, 0, SendState, this);
        PollItem poller = new PollItem(statesub, ZMQ.Poller.POLLIN);
        loop.addPoller(poller, RecvState, this);
    }

    //  .split destructor
    //  The destructor shuts down the bstar reactor:
    public void destroy()
    {
        loop.destroy();
        ctx.destroy();
    }

    //  .split zloop method
    //  This method returns the underlying zloop reactor, so we can add
    //  additional timers and readers:
    public ZLoop zloop()
    {
        return loop;
    }

    //  .split voter method
    //  This method registers a client voter socket. Messages received
    //  on this socket provide the CLIENT_REQUEST events for the Binary Star
    //  FSM and are passed to the provided application handler. We require
    //  exactly one voter per {{bstar}} instance:
    public int voter(String endpoint, SocketType type, IZLoopHandler handler, Object arg)
    {
        //  Hold actual handler+arg so we can call this later
        Socket socket = ctx.createSocket(type);
        socket.bind(endpoint);
        voterFn = handler;
        voterArg = arg;
        PollItem poller = new PollItem(socket, ZMQ.Poller.POLLIN);
        return loop.addPoller(poller, VoterReady, this);
    }

    //  .split register state-change handlers
    //  Register handlers to be called each time there's a state change:
    public void newActive(IZLoopHandler handler, Object arg)
    {
        activeFn = handler;
        activeArg = arg;
    }

    public void newPassive(IZLoopHandler handler, Object arg)
    {
        passiveFn = handler;
        passiveArg = arg;
    }

    //  .split enable/disable tracing
    //  Enable/disable verbose tracing, for debugging:
    public void setVerbose(boolean verbose)
    {
        loop.verbose(verbose);
    }

    //  .split start the reactor
    //  Finally, start the configured reactor. It will end if any handler
    //  returns -1 to the reactor, or if the process receives Interrupt

    public int start()
    {
        assert (voterFn != null);
        updatePeerExpiry();
        return loop.start();
    }
}
