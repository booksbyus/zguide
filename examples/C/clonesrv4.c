//
//  Clone server model 4
//  Uses bstar and clone classes
//

//  Lets us build this source without creating a library
#include "bstar.c"
#include "clone.c"

static int s_snapshotter (zloop_t *loop, void *socket, void *args);
static int s_collector (zloop_t *loop, void *socket, void *args);
static int s_send_hugz (zloop_t *loop, void *socket, void *args);
static int s_failover (zloop_t *loop, void *unused, void *args);

//  Our server is defined by these properties
typedef struct {
    zctx_t *ctx;                //  Context wrapper
    zhash_t *kvmap;             //  Key-value store
    bstar_t *bstar;             //  Bstar reactor core
    clone_t *clone;             //  Clone client class
    int64_t sequence;           //  How many updates we're at
    char *port;                 //  Main port we're working on
    char *peer;                 //  Main port of our peer
    void *publisher;            //  Publishing updates and hugz
    void *collector;            //  Collecting updates from clients
} clonesrv_t;

int main (int argc, char *argv [])
{
    clonesrv_t *self = (clonesrv_t *) zmalloc (sizeof (clonesrv_t));
    if (argc == 2 && streq (argv [1], "-p")) {
        printf ("I: Primary master, waiting for backup (slave)\n");
        self->bstar = bstar_new (BSTAR_PRIMARY, "tcp://*:5003",
                                 "tcp://localhost:5004");
        bstar_voter (self->bstar, "tcp://*:5556", ZMQ_ROUTER,
                     s_snapshotter, self);
        self->port = "5556";
        self->peer = "5566";
    }
    else
    if (argc == 2 && streq (argv [1], "-b")) {
        printf ("I: Backup slave, waiting for primary (master)\n");
        self->bstar = bstar_new (BSTAR_BACKUP, "tcp://*:5004",
                                 "tcp://localhost:5003");
        bstar_voter (self->bstar, "tcp://*:5566", ZMQ_ROUTER,
                     s_snapshotter, self);
        self->port = "5566";
        self->peer = "5556";
    }
    else {
        printf ("Usage: clonesrv4 { -p | -b }\n");
        free (self);
        exit (0);
    }
    self->kvmap = zhash_new ();
    self->ctx = zctx_new ();
    self->publisher = zsocket_new (self->ctx, ZMQ_PUB);
    self->collector = zsocket_new (self->ctx, ZMQ_SUB);
    zsocket_bind (self->publisher, "tcp://*:%d", atoi (self->port) + 1);
    zsocket_bind (self->collector, "tcp://*:%d", atoi (self->port) + 2);

    //  Launch clone client against peer server
    self->clone = clone_new ();
    clone_connect (self->clone, "tcp://localhost", self->peer);

    //  Register failover handler
    bstar_failover (self->bstar, s_failover, self);

    //  Register our other handlers with the bstar reactor
    zloop_t *loop = bstar_zloop (self->bstar);
    zloop_reader (loop, self->collector, s_collector, self);
    zloop_timer (loop, 1000, 0, s_send_hugz, self);

    //  Start the Bstar reactor
    bstar_start (self->bstar);

    //  Interrupted, so shut down
    clone_destroy (&self->clone);
    bstar_destroy (&self->bstar);
    zhash_destroy (&self->kvmap);
    zctx_destroy (&self->ctx);
    free (self);

    return 0;
}


//  ---------------------------------------------------------------------
//  Snapshot handler

static int s_send_kvmsg (char *key, void *data, void *args);

//  Routing information for a KV snapshot
typedef struct {
    void *socket;           //  ROUTER socket to send to
    zframe_t *identity;     //  Identity of peer who requested state
} kvroute_t;

static int
s_snapshotter (zloop_t *loop, void *socket, void *args)
{
    clonesrv_t *self = (clonesrv_t *) args;

    zframe_t *identity = zframe_recv (socket);
    zframe_t *request = zframe_recv (socket);
    assert (zframe_streq (request, "ICANHAZ?"));
    zframe_destroy (&request);

    //  Send state socket to client
    kvroute_t routing = { socket, identity };
    zhash_foreach (self->kvmap, s_send_kvmsg, &routing);

    //  Now send END message with sequence number
    printf ("I: sending shapshot=%" PRId64 "\n", self->sequence);
    zframe_send (&identity, socket, ZFRAME_MORE);
    kvmsg_t *kvmsg = kvmsg_new (self->sequence);
    kvmsg_set_key  (kvmsg, "KTHXBAI");
    kvmsg_set_body (kvmsg, (byte *) "", 0);
    kvmsg_send     (kvmsg, socket);
    kvmsg_destroy (&kvmsg);
    return 0;
}

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


//  ---------------------------------------------------------------------
//  Collector

static int
s_collector (zloop_t *loop, void *socket, void *args)
{
    clonesrv_t *self = (clonesrv_t *) args;

    kvmsg_t *kvmsg = kvmsg_recv (socket);
    kvmsg_set_sequence (kvmsg, ++self->sequence);
    kvmsg_send (kvmsg, self->publisher);
    kvmsg_store (&kvmsg, self->kvmap);
    printf ("I: publishing update %5" PRId64 "\n", self->sequence);

    return 0;
}


//  ---------------------------------------------------------------------
//  Send hugz to anyone listening on the publisher socket

static int
s_send_hugz (zloop_t *loop, void *socket, void *args)
{
    clonesrv_t *self = (clonesrv_t *) args;

    kvmsg_t *kvmsg = kvmsg_new (self->sequence);
    kvmsg_set_key  (kvmsg, "HUGZ");
    kvmsg_set_body (kvmsg, (byte *) "", 0);
    kvmsg_send     (kvmsg, self->publisher);
    kvmsg_destroy (&kvmsg);

    return 0;
}


//  ---------------------------------------------------------------------
//  Failover handler
//
//  The backup server applies its pending list to its own hash table,
//  and then starts to process state snapshot requests.
//   - pending list, how
//  The backup server keeps a "pending list" of updates that it has
//  received from clients, but not yet from the primary server. The
//  list is ordered from oldest to newest, so that it is easy to remove
//  updates off the head.

static int
s_failover (zloop_t *loop, void *unused, void *args)
{
    clonesrv_t *self = (clonesrv_t *) args;

    puts ("FAILOVER SUCCESSFUL!");

    return 0;
}
