//
//  Clone server model 2
//
#include "kvmsg.h"

//  Routing information for a KV snapshot
typedef struct {
    void *socket;           //  ROUTER socket to send to
    zmq_msg_t *identity;    //  Identity of peer who requested state
    int64_t sequence;       //  Sequence of whole state snapshot
} kvroute_t;

int
send_kvpair (char *key, void *data, void *args)
{
    kvroute_t *kvroute = (kvroute *) args;

    kvmsg_t *kvmsg = kvmsg_new ();
    kvmsg_set_sequence (kvmsg, kvroute->sequence);
    kvmsg_set_key  (kvmsg, "%s", key);
    kvmsg_set_body (kvmsg, "%d", data);
    kvmsg_send (&kvmsg, publisher);
    
    return 0;
}



//  This thread maintains the state and handles requests from clients
//  for snapshots.
//  
static void *
state_manager (void *context) 
{
    zhash_t *kvmap = zhash_new ();
    
    void *updates = zmq_socket (context, ZMQ_PAIR);
    int rc = zmq_connect (updates, "inproc://updates");
    assert (rc == 0);

    void *snapshots = zmq_socket (context, ZMQ_ROUTER);
    rc = zmq_bind (snapshots, "tcp://*:5557");
    assert (rc == 0);
    
    zmq_pollitem_t items [] = { 
        { updates, 0, ZMQ_POLLIN, 0 },
        { snapshots, 0, ZMQ_POLLIN, 0 } 
    };
    int64_t sequence = 0;       //  State version number
    while (!s_interrupted) {
        int rc = zmq_poll (items, 2, -1);
        if (rc == -1 && errno == ETERM)
            break;              //  Context has been shut down

        //  Apply state update from main thread
        if (items [0].revents & ZMQ_POLLIN) {
            kvmsg_t *kvmsg = kvmsg_recv (updates);
            if (!kvmsg)
                break;          //  Interrupted
            kvmsg_hash_put (kvmsg, kvmap);
            sequence = kvmsg_sequence (kvmsg);
            kvmsg_destroy (&kvmsg);
        }
        //  Execute state snapshot request
        if (items [1].revents & ZMQ_POLLIN) {
            zmq_msg_t message;
            zmq_msg_init (&message);
            if (zmq_recv (socket, &message, 0))
                break;          //  Interrupted
            zhash_apply (kvmap, send_kvpair, &);
            
            
            zmq_msg_close (&message);
        }
    }
    zhash_destroy (&kvmap);
}


int main (void) 
{
    //  Prepare our context and publisher
    void *context = zmq_init (1);
    void *publisher = zmq_socket (context, ZMQ_PUB);
    zmq_bind (publisher, "tcp://*:5556");

    s_catch_signals ();
    int64_t sequence = 0;
    srandom ((unsigned) time (NULL));

    void *updates = zmq_socket (context, ZMQ_PAIR);
    int rc = zmq_bind (updates, "inproc://updates");
    assert (rc == 0);
    
    
    
    
    pthread_t thread;
    pthread_create (&thread, NULL, state_manager, context);
    pthread_detach (thread);
    
    while (!s_interrupted) {
        //  Distribute as key-value message
        kvmsg_t *kvmsg = kvmsg_new ();
        kvmsg_set_sequence (kvmsg, sequence++);
        kvmsg_fmt_key  (kvmsg, "%d", randof (10000));
        kvmsg_fmt_body (kvmsg, "%d", randof (1000000));
        kvmsg_hash_put (kvmsg, kvmap);
        kvmsg_send (&kvmsg, publisher);
    }
    printf (" Interrupted\n%" PRId64 " messages out\n", sequence);
    zmq_close (publisher);
    zmq_term (context);
    return 0;
}
