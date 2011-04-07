//
//  Clone server model 4
//  Uses bstar and clone classes
//

//  Lets us build this source without creating a library
#include "kvmsg.c"
#include "bstar.c"
#include "clone.c"

static int s_send_kvmsg (char *key, void *data, void *args);
static int s_snapshot (zloop_t *loop, void *socket, void *arg);

int main (int argc, char *argv [])
{
    if (argc == 2 && streq (argv [1], "-p")) {
        role = BSTAR_PRIMARY;
        port = 5556;
    }
    else
    if (argc == 2 && streq (argv [1], "-b")) {
        role = BSTAR_BACKUP;
        port = 5566;
    }
    else {
        printf ("Usage: clonesrv4 { -p | -b }\n");
        exit (0);
    }
    char endpoint [64];

    bstar_t *bstar;
    zctx_t *ctx = zctx_new ();

    void *publisher = zsocket_new (ctx, ZMQ_PUB);
    zsocket_bind (publisher, "tcp://*", port);
    void *collector = zsocket_new (ctx, ZMQ_SUB);
    zsocket_bind (collector, "tcp://*", port + 1);

    void *publisher = zsocket_new (ctx, ZMQ_PUB);
    void *collector = zsocket_new (ctx, ZMQ_SUB);

        printf ("I: Primary master, waiting for backup (slave)\n");
        bstar = bstar_new (BSTAR_PRIMARY,
            "tcp://*:5003", "tcp://localhost:5004");
        bstar_voter (bstar, "tcp://*:5556", ZMQ_ROUTER, s_snapshot, NULL);
        int rc = zmq_bind (publisher, "tcp://*:5557");
        assert (rc == 0);
        rc = zmq_bind (collector, "tcp://*:5558");
        assert (rc == 0);
    }
    else
    if (argc == 2 && streq (argv [1], "-b")) {
        printf ("I: Backup slave, waiting for primary (master)\n");
        bstar = bstar_new (BSTAR_BACKUP,
            "tcp://*:5004", "tcp://localhost:5003");
        bstar_voter (bstar, "tcp://*:5566", ZMQ_ROUTER, s_snapshot, NULL);
        int rc = zmq_bind (publisher, "tcp://*:5567");
        assert (rc == 0);
        rc = zmq_bind (collector, "tcp://*:5568");
        assert (rc == 0);
    }

    bstar_start (bstar);

    //  Now handle activity from clients
    bstar thread
        - comes back when we have snapshot request from client
        - can get this from second thread

    pubsub thread
        - handles incoming updates, outgoing updates, and hugz


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


    //  Prepare our context and sockets
    void *publisher = zsocket_new (ctx, ZMQ_PUB);
    int rc = zmq_bind (publisher, "tcp://*:5556");
    assert (rc == 0);

    void *snapshot = zsocket_new (ctx, ZMQ_ROUTER);
    rc = zmq_bind (snapshot, "tcp://*:5557");
    assert (rc == 0);

    void *collector = zsocket_new (ctx, ZMQ_PULL);
    rc = zmq_bind (collector, "tcp://*:5558");
    assert (rc == 0);


    //  Publisher thread
    void *publisher = zsocket_new (ctx, ZMQ_PUB);
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
            kvmsg_store (&kvmsg, kvmap);
            printf ("I: publishing update %5" PRId64 "\n", sequence);
        }
        //  Execute state snapshot request
        if (items [1].revents & ZMQ_POLLIN) {
        }
        //  If we timed-out, send hugz to all clients
        //  ...do with timer handler in bstar class...
        if (s_clock () >= alarm) {
            kvmsg_t *kvmsg = kvmsg_new (sequence);
            kvmsg_set_key  (kvmsg, "HUGZ");
            kvmsg_set_body (kvmsg, (byte *) "", 0);
            kvmsg_send (kvmsg, publisher);
            kvmsg_destroy (&kvmsg);
            alarm = s_clock () + 1000;
        }
    }
    printf (" Interrupted\n%" PRId64 " messages handled\n", sequence);
    zhash_destroy (&kvmap);
    zctx_destroy (&ctx);

    return 0;
}

//  Routing information for a KV snapshot
typedef struct {
    void *socket;           //  ROUTER socket to send to
    zframe_t *identity;     //  Identity of peer who requested state
} kvroute_t;

//  Send one state snapshot key-value pair to a socket
//  Hash item data is our kvmsg object, ready to send
static int
s_send_kvmsg (char *key, void *data, void *args)
{
    kvroute_t *kvroute = (kvroute_t *) args;
    //  Send identity of recipient first
    zframe_send (&kvroute->identity,
        kvroute->socket, ZFRAME_MORE + ZFRAME_REUSE);
    kvmsg_t *kvmsg = (kvmsg_t *) data;
    kvmsg_send (kvmsg, kvroute->socket);
    return 0;
}

//  Snapshot handler
static int
s_snapshot (zloop_t *loop, void *socket, void *arg)
{
    zframe_t *identity = zframe_recv (socket);
    if (!identity)
        break;          //  Interrupted

    //  Request is in second frame of message
    zframe_t *request = zframe_recv (socket);
    if (zframe_streq (request, "ICANHAZ?"))
        zframe_destroy (&request);
    else {
        printf ("E: bad request, aborting\n");
        break;
    }
    //  Send state socket to client
    kvroute_t routing = { socket, identity };

    //  For each entry in kvmap, send kvmsg to client
    zhash_foreach (kvmap, s_send_kvmsg, &routing);

    //  Now send END message with sequence number
    printf ("I: sending shapshot=%" PRId64 "\n", sequence);
    zframe_send (&identity, socket, ZFRAME_MORE);
    kvmsg_t *kvmsg = kvmsg_new (sequence);
    kvmsg_set_key  (kvmsg, "KTHXBAI");
    kvmsg_set_body (kvmsg, (byte *) "", 0);
    kvmsg_send (kvmsg, socket);
    kvmsg_destroy (&kvmsg);
    return 0;
}

