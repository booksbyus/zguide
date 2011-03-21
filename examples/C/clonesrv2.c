//
//  Clone server model 2
//
#include "kvmsg.h"
#include "zmsg.h"

//  Routing information for a KV snapshot
typedef struct {
    void *socket;           //  ROUTER socket to send to
    zmq_msg_t *identity;    //  Identity of peer who requested state
} kvroute_t;

//  Send one state snapshot key-value pair to a socket
//  Hash item data is our kvmsg object, ready to send
int
send_one_kvmsg (char *key, void *data, void *args)
{
    kvroute_t *kvroute = (kvroute_t *) args;
    //  Send identity of recipient first
    zmq_msg_t copy;
    zmq_msg_init (&copy);
    zmq_msg_copy (&copy, kvroute->identity);
    zmq_send (kvroute->socket, &copy, ZMQ_SNDMORE);
    zmq_msg_close (&copy);

    kvmsg_t *kvmsg = (kvmsg_t *) data;
    kvmsg_send (kvmsg, kvroute->socket);
    return 0;
}

//  This thread maintains the state and handles requests from
//  clients for snapshots.
//  
static void *
state_manager (void *context) 
{
    zhash_t *kvmap = zhash_new ();

    void *updates = zmq_socket (context, ZMQ_PAIR);
    int rc = zmq_connect (updates, "inproc://updates");
    assert (rc == 0);
    s_send (updates, "Ready");

    void *snapshot = zmq_socket (context, ZMQ_ROUTER);
    rc = zmq_bind (snapshot, "tcp://*:5557");
    assert (rc == 0);
    
    zmq_pollitem_t items [] = { 
        { updates, 0, ZMQ_POLLIN, 0 },
        { snapshot, 0, ZMQ_POLLIN, 0 } 
    };
    int64_t sequence = 0;       //  Current snapshot version number
    while (!s_interrupted) {
        int rc = zmq_poll (items, 2, -1);
        if (rc == -1 && errno == ETERM)
            break;              //  Context has been shut down

        //  Apply state update from main thread
        if (items [0].revents & ZMQ_POLLIN) {
            kvmsg_t *kvmsg = kvmsg_recv (updates);
            if (!kvmsg)
                break;          //  Interrupted
            kvmsg_store (kvmsg, kvmap);
            sequence = kvmsg_sequence (kvmsg);
        }
        //  Execute state snapshot request
        if (items [1].revents & ZMQ_POLLIN) {
            zmq_msg_t identity;
            zmq_msg_init (&identity);
            if (zmq_recv (snapshot, &identity, 0))
                break;          //  Interrupted
            //  Get and discard second frame of message
            zmq_msg_t icanhaz;
            zmq_msg_init (&icanhaz);
            if (zmq_recv (snapshot, &icanhaz, 0))
                break;          //  Interrupted
            zmq_msg_close (&icanhaz);

            //  Send state snapshot to client
            kvroute_t routing = { snapshot, &identity };
            //  For each entry in kvmap, send kvmsg to client
            zhash_foreach (kvmap, send_one_kvmsg, &routing);
            
            //  Now send END message with sequence number
            zmq_send (snapshot, &identity, ZMQ_SNDMORE);
            zmq_msg_close (&identity);
            
            printf ("Sending state sequence=%" PRId64 "\n", sequence);
            kvmsg_t *kvmsg = kvmsg_new (sequence);
            kvmsg_set_key  (kvmsg, "KTHXBAI");
            kvmsg_set_body (kvmsg, (byte *) "", 0);
            kvmsg_send (kvmsg, snapshot);
            kvmsg_destroy (&kvmsg);
        }
    }
    zhash_destroy (&kvmap);
    zmq_close (updates);
    zmq_close (snapshot);
    return NULL;
}


int main (void) 
{
    //  Prepare our context and sockets
    void *context = zmq_init (1);
    void *publisher = zmq_socket (context, ZMQ_PUB);
    int rc = zmq_bind (publisher, "tcp://*:5556");
    assert (rc == 0);
    
    void *updates = zmq_socket (context, ZMQ_PAIR);
    rc = zmq_bind (updates, "inproc://updates");
    assert (rc == 0);

    s_catch_signals ();
    int64_t sequence = 0;
    srandom ((unsigned) time (NULL));

    //  Start state manager and wait for synchronization signal
    pthread_t thread;
    pthread_create (&thread, NULL, state_manager, context);
    pthread_detach (thread);
    free (s_recv (updates));
    
    while (!s_interrupted) {
        //  Distribute as key-value message
        kvmsg_t *kvmsg = kvmsg_new (++sequence);
        kvmsg_fmt_key  (kvmsg, "%d", randof (10000));
        kvmsg_fmt_body (kvmsg, "%d", randof (1000000));
        kvmsg_send (kvmsg, publisher);
        kvmsg_send (kvmsg, updates);
        kvmsg_destroy (&kvmsg);
    }
    printf (" Interrupted\n%" PRId64 " messages out\n", sequence);
    zmq_close (publisher);
    zmq_close (updates);
    zmq_term (context);
    return 0;
}
