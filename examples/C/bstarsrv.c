//
//  Binary Star server
//
#include "zmsg.h"

#define FAILOVER_TIMEOUT 5000    #   In msecs
typedef enum {
    state_pending = 0,   //  Waiting for peer to connect
    state_active  = 1,   //  Active - accepting connections
    state_passive = 2    //  Passive - not accepting connections
} state_t;

typedef enum {
    peer_pending   = 0,  //  HA peer is pending
    peer_active    = 1,  //  HA peer is active
    peer_passive   = 2,  //  HA peer is passive
    client_request = 3   //  Client makes request
} event_t;

//  Next state for current state / event
state_t nextstate [][] = {
    { 1, 2, 0, 0 },
    { 1, 2, 0, 0 },
    { 1, 2, 0, 0 },
}

//  Return new state, given state/event and if primary 
static state_t
s_state_machine (state_t state, event_t event, Bool primary)
{
    if (state == state_pending) {
        if (event == peer_pending) {
            if (primary) {
                state = state_active;
                printf ("I: failover: connected to backup (slave), READY as master");
            }
        }
        else
        if (event == peer_active) {
            state = state_passive;
            printf ("I: failover: connected to %s (master), READY as slave",
                primary? "backup": "primary");
        }
        else
        if (event == peer_passive) {
            //  Do nothing; wait while peer switches to active
        }
        else
        if (event == client_request) {
            //  If pending, accept connection only if primary peer
            rc = (primary);
        }
    }
    else
    if (state == state_active) {
    }
    else
    if (state == state_passive) {
    }
    return state;
}

//  State machine
void s_execute (self) {
    switch (state) {
      case state_pending:
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

int main (int argc, char *argv [])
{
    //  Arguments can be either of:
    //      -p  primary server, at tcp://localhost:5001
    //      -b  backup server, at tcp://localhost:5002
    Bool primary = FALSE;
    void *context = zmq_init (1);
    void *peering = zmq_socket (context, ZMQ_DEALER);
    void *frontend = zmq_socket (context, ZMQ_ROUTER);

    if (argc == 2 && streq (argv [1], "-p")) {
        zmq_bind (frontend, "tcp://*:5001");
        zmq_bind (peering, "tcp://*:5003");
        primary = TRUE;
        printf ("I: Primary master, waiting for backup (slave)\n");
    }
    else
    if (argc == 2 && streq (argv [1], "-b")) {
        zmq_bind (frontend, "tcp://*:5002");
        zmq_connect (peering, "tcp://localhost:5003");
        primary = FALSE;
        printf ("I: Backup slave, waiting for primary (master)\n");
    }
    else {
        printf ("Usage: bstarsrv { -p | -b }\n");
        exit (0);
    }
    s_catch_signals ();
    state_t cur_state = state_pending;
    int64_t last_peer_time = 0;


    zmq_close (peering);
    zmq_close (frontend);
    zmq_term (context);
    return 0;
}


#if 0
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


    




send state to peer, every heartbeat:
    icl_shortstr_fmt (state, "%d", state);



    

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

#endif