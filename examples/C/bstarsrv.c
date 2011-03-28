//
//  Binary Star server
//
#include "zmsg.h"

#define FAILOVER_TIMEOUT 5000    #   In msecs

int main (int argc, char *argv [])
{
    //  Arguments can be either of:
    //      -p  primary server
    //      -b  backup server
    int primary = 0;
    void *context = zmq_init (1);
    void *peering = zmq_socket (context, ZMQ_DEALER);
    void *frontend = zmq_socket (context, ZMQ_ROUTER);

    if (argc == 2 && strcmp (argv [2], "-p") == 0) {
        zmq_bind (frontend, "tcp://*:5001");
        zmq_bind (peering, "tcp://*:5003");
        primary = 1;
        printf ("I: Primary master, waiting for backup (slave)\n");
    }
    else
    if (argc == 2 && strcmp (argv [2], "-b") == 0) {
        zmq_bind (frontend, "tcp://*:5002");
        zmq_connect (peering, "tcp://localhost:5003");
        primary = 0;
        printf ("I: Backup slave, waiting for primary (master)\n");
    }
    else {
        printf ("Usage: bstarsrv { -p | -b }\n");
        exit (0);
    }
    s_catch_signals ();
    int state = state_pending;
    int64_t last_peer_time = 0;

    while (!s_interrupted) {
        //  poll tickless 1 second
        //  appl message on frontend
        //      - check if valid in current state
        //  peer state
        //      - process in current state

        
        zmsg_t *request = zmsg_recv (server);
        zmsg_t *reply = NULL;
        if (verbose && request)
            zmsg_dump (request);
        if (!request)
            break;          //  Interrupted

        //  Frame 0: identity of client
        //  Frame 1: PING, or client control frame
        //  Frame 2: request body
        char *address = zmsg_pop (request);
        if (zmsg_parts (request) == 1
        && strcmp (zmsg_body (request), "PING") == 0)
            reply = zmsg_new ("PONG");
        else
        if (zmsg_parts (request) > 1) {
            reply = request;
            request = NULL;
            zmsg_body_set (reply, "OK");
        }
        zmsg_destroy (&request);
        zmsg_push (reply, address);
        if (verbose && reply)
            zmsg_dump (reply);
        zmsg_send (&reply, server);
        free (address);
    }
    if (s_interrupted)
        printf ("W: interrupted\n");

    zmq_close (server);
    zmq_term (context);
    return 0;
}





    
typedef enum
{
    state_pending = 1,   //  Waiting for peer to connect
    state_active  = 2,   //  Active - accepting connections
    state_passive = 3    //  Passive - not accepting connections
} state;

typedef enum
{
    event_peer_pending   = 1,  //  HA peer became pending
    event_peer_active    = 2,  //  HA peer became active
    event_peer_passive   = 3,  //  HA peer became passive
    event_new_connection = 4   //  Client wants to connect
} event;



send state to peer, every heartbeat:
    icl_shortstr_fmt (state, "%d", state);


//  State machine
void execute (self) {
    switch (state) {
      case state_pending:
        switch (event) {
          case event_peer_pending:
            if (primary) {
                state = state_active;
                printf ("I: failover: connected to backup (slave), READY as master");
            }
            break;
          case event_peer_active:
            state = state_passive;
            printf ("I: failover: connected to %s (master), READY as slave",
                primary? "backup": "primary");
            break;
          case event_peer_passive:
            //  Do nothing; wait while peer switches to active
            break;
          case event_new_connection:
            //  If pending, accept connection only if primary peer
            rc = (primary);
            break;
          default:
            assert (0);
        }
        break;

      case state_active:
        switch (event) {
          case event_peer_pending:
            //  Do nothing; slave is starting
            break;
          case event_peer_active:
            //  No way to have two masters - that would mean split-brain
            printf ("E: failover: fatal error - dual masters detected, aborting");
            assert (0);
            break;
          case event_peer_passive:
            //  Do nothing; everything is OK
            break;
          case event_new_connection:
            //  Active state, we do accept new connections
            rc = 1;
            break;
          default:
            assert (0);
        }
        break;

      case state_passive:
        switch (event) {
          case event_peer_pending:
            //  The peer is restarting; become active (peer will become passive)
            state = state_active;
            printf ("I: failover: %s (slave) is restarting, READY as master",
                primary? "backup": "primary");
            break;
          case event_peer_active:
            //  Do nothing; everything is OK
            break;
          case event_peer_passive:
            //  No way to have two passives - cluster would be non-responsive
            printf ("E: failover: fatal error - dual slaves, aborting");
            assert (0);
            break;
          case event_new_connection:
            //  Peer becomes master if timeout has passed
            //  It's the connection request that triggers the failover
            if (smt_time_now () - last_peer_time > timeout) {
                //  If peer is dead, switch to the active state
                state = state_active;
                printf ("I: failover: failover successful, READY as master");
                rc = 1;                 //  Accept the request, then
            }
            else
                //  If peer is alive, reject connections
                rc = 0;
            break;
          default:
            assert (0);
        }
        break;

      default:
        assert (0);
    }
}

    

    amq_peering_t
        *peering;                       //  The peering to the other HA peer
    Bool
        enabled,                        //  If FALSE, broker is standalone
        primary;                        //  TRUE = primary, FALSE = backup
    long
        timeout;                        //  Failover timeout in usec
    state
        state;                          //  State of failover FSM
    apr_time_t
        last_peer_time;                 //  Time when peer state arrived lately
                                        //  If this time is older than the failover
                                        //  timeout, the peer is considered dead



    //  --------------- message handling --------------------------
    
    //  We got something from peer, record time
    last_peer_time = time_now ();

    //  Parse content
    state = atoi ((char *) body->data);
    assert (state != 0);

    //  Convert peer's state to FSM event
    //  This isn't really great design...
    switch (state) {
        case state_pending:
            event = event_peer_pending;
            break;
        case state_active:
            event = event_peer_active;
            break;
        case state_passive:
            event = event_peer_passive;
            break;
        default:
            assert (0);
    }
    //  Run the FSM
    execute (self, event);
}

