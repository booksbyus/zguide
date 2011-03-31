//
//  Clone server model 3
//

//  Lets us build this source without creating a library
#include "kvmsg.c"

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

int main (int argc, char *argv [])
{
    //  Arguments can be either of:
    //      -p  primary server, at tcp://localhost:5551
    //      -b  backup server, at tcp://localhost:5561
    bstar_t *bstar;
    if (argc == 2 && streq (argv [1], "-p")) {
        printf ("I: Primary master, waiting for backup (slave)\n");
        bstar = bstar_new (BSTAR_PRIMARY,
            "tcp://*:5003", "tcp://localhost:5004");
        bstar_listen (bstar, "tcp://*:5551", ZMQ_ROUTER);
        service = 5551;
    }
    else
    if (argc == 2 && streq (argv [1], "-b")) {
        printf ("I: Backup slave, waiting for primary (master)\n");
        bstar = bstar_new (BSTAR_BACKUP,
            "tcp://*:5004", "tcp://localhost:5003");
        bstar_listen (bstar, "tcp://*:5561", ZMQ_ROUTER);
        service = 5561;
    }
    else {
        printf ("Usage: clonesrv4 { -p | -b }\n");
        exit (0);
    }
    //  Now handle activity from clients
    bstar thread
        - comes back when we have snapshot request from client
        - can get this from second thread

    pubsub thread
        - handles incoming updates, outgoing updates, and hugz

    void *collector = zmq_socket (context, ZMQ_SUB);
    rc = zmq_bind (collector, "tcp://*:xxxx");
    void *publisher = zmq_socket (context, ZMQ_PUB);
    int rc = zmq_bind (publisher, "tcp://*:xxxx");

    int64_t sequence = 0;
    zhash_t *kvmap = zhash_new ();
    while (!s_interrupted) {
        void *socket = bstar_wait (bstar);
        if (!socket)
            break;              //  Interrupted, or fatal error

        int socket_type;
        size_t type_size = sizeof (socket_type);
        zmq_getsockopt (socket, ZMQ_TYPE, &socket_type, &type_size);
        if (socket_type == ZMQ_ROUTER) {

        }
        else
        if (socket_type == ZMQ_SUB) {

        }
    }
    bstar_destroy (&bstar);



    //  Publisher thread
    void *publisher = zmq_socket (context, ZMQ_PUB);
    int rc = zmq_bind (publisher, publisher_endpoint);
    zmq_pollitem_t items [] = {
        { subscriber, 0, ZMQ_POLLIN, 0 },
        { snapshot, 0, ZMQ_POLLIN, 0 }
    };
    int64_t alarm = s_clock () + 1000;
    while (!s_interrupted) {
        int tickless = (int) ((alarm - s_clock ()));
        if (tickless < 0)
            tickless = 0;
        int rc = zmq_poll (items, 2, tickless * 1000);

        //  Apply state update sent from client
        if (items [0].revents & ZMQ_POLLIN) {
            kvmsg_t *kvmsg = kvmsg_recv (subscriber);
            if (!kvmsg)
                break;          //  Interrupted
            kvmsg_set_sequence (kvmsg, ++sequence);
            kvmsg_send (kvmsg, publisher);
            printf ("I: publishing update %5" PRId64 "\n", sequence);
            kvmsg_store (&kvmsg, kvmap);
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
            kvmsg_fmt_key  (kvmsg, "KTHXBAI");
            kvmsg_fmt_body (kvmsg, "%ld", ++client_id);
            kvmsg_send (kvmsg, snapshot);
            kvmsg_destroy (&kvmsg);
        }
        //  If we timed-out, send hugz to all clients
        //  ...do with timer handler in bstar class...
        if (s_clock () >= alarm) {
            kvmsg_t *kvmsg = kvmsg_new (sequence);
            kvmsg_fmt_key  (kvmsg, "HUGZ");
            kvmsg_fmt_body (kvmsg, "");
            kvmsg_send (kvmsg, publisher);
            kvmsg_destroy (&kvmsg);
            alarm = s_clock () + 1000;
        }
    }
    printf (" Interrupted\n%" PRId64 " messages handled\n", sequence);
    zhash_destroy (&kvmap);
    zmq_close (publisher);
    zmq_close (subscriber);
    zmq_close (snapshot);
    zmq_term (context);

    return 0;
}
