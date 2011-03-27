//
//  Clone server model 3
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

int main (void) 
{
    //  Prepare our context and sockets
    void *context = zmq_init (1);
    void *publisher = zmq_socket (context, ZMQ_PUB);
    int rc = zmq_bind (publisher, "tcp://*:5556");
    assert (rc == 0);

    void *snapshot = zmq_socket (context, ZMQ_ROUTER);
    rc = zmq_bind (snapshot, "tcp://*:5557");
    assert (rc == 0);
    
    void *collector = zmq_socket (context, ZMQ_PULL);
    uint64_t hwm = 1;
    zmq_setsockopt (collector, ZMQ_HWM, &hwm, sizeof (hwm));
    rc = zmq_bind (collector, "tcp://*:5558");
    assert (rc == 0);

    s_catch_signals ();
    int64_t sequence = 0;
    zhash_t *kvmap = zhash_new ();

    zmq_pollitem_t items [] = { 
        { collector, 0, ZMQ_POLLIN, 0 },
        { snapshot, 0, ZMQ_POLLIN, 0 } 
    };
    while (!s_interrupted) {
        int rc = zmq_poll (items, 2, 1000 * 1000);

        //  Apply state update sent from client
        if (items [0].revents & ZMQ_POLLIN) {
            kvmsg_t *kvmsg = kvmsg_recv (collector);
            if (!kvmsg)
                break;          //  Interrupted
            kvmsg_set_sequence (kvmsg, ++sequence);
            kvmsg_send (kvmsg, publisher);
            kvmsg_store (&kvmsg, kvmap);
            printf ("I: publishing update %" PRId64 "\n", sequence);
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
            printf ("I: sending shapshot=%" PRId64 "\n", sequence);
            zmq_send (snapshot, &identity, ZMQ_SNDMORE);
            zmq_msg_close (&identity);
            kvmsg_t *kvmsg = kvmsg_new (sequence);
            kvmsg_set_key  (kvmsg, "KTHXBAI");
            kvmsg_set_body (kvmsg, (byte *) "", 0);
            kvmsg_send (kvmsg, snapshot);
            kvmsg_destroy (&kvmsg);
        }
    }
    printf (" Interrupted\n%" PRId64 " messages handled\n", sequence);
    zhash_destroy (&kvmap);
    zmq_close (publisher);
    zmq_close (collector);
    zmq_close (snapshot);
    zmq_term (context);
    
    return 0;
}
