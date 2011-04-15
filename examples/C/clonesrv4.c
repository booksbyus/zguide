//
//  Clone server model 4
//  Uses bstar and clone classes
//

//  Lets us build this source without creating a library
#include "bstar.c"
#include "kvmsg.c"

//  Bstar reactor handlers
static int s_send_snapshot (zloop_t *loop, void *socket, void *args);
static int s_collector     (zloop_t *loop, void *socket, void *args);
static int s_send_hugz     (zloop_t *loop, void *socket, void *args);
static int s_new_master    (zloop_t *loop, void *unused, void *args);
static int s_new_slave     (zloop_t *loop, void *unused, void *args);
static int s_subscriber    (zloop_t *loop, void *socket, void *args);

//  Our server is defined by these properties
typedef struct {
    zctx_t *ctx;                //  Context wrapper
    zhash_t *kvmap;             //  Key-value store
    bstar_t *bstar;             //  Bstar reactor core
    int64_t sequence;           //  How many updates we're at
    int port;                   //  Main port we're working on
    int peer;                   //  Main port of our peer
    void *publisher;            //  Publishing updates and hugz
    void *collector;            //  Collecting updates from clients
    void *subscriber;           //  Getting updates from peer
    zlist_t *pending;           //  Pending updates from clients
    Bool primary;               //  TRUE if we're primary
    Bool master;                //  TRUE if we're master
    Bool slave;                 //  TRUE if we're slave
} clonesrv_t;


int main (int argc, char *argv [])
{
    clonesrv_t *self = (clonesrv_t *) zmalloc (sizeof (clonesrv_t));
    if (argc == 2 && streq (argv [1], "-p")) {
        zclock_log ("I: primary master, waiting for backup (slave)");
        self->bstar = bstar_new (BSTAR_PRIMARY, "tcp://*:5003",
                                 "tcp://localhost:5004");
        bstar_voter (self->bstar, "tcp://*:5556", ZMQ_ROUTER,
                     s_send_snapshot, self);
        self->port = 5556;
        self->peer = 5566;
        self->primary = TRUE;
    }
    else
    if (argc == 2 && streq (argv [1], "-b")) {
        zclock_log ("I: backup slave, waiting for primary (master)");
        self->bstar = bstar_new (BSTAR_BACKUP, "tcp://*:5004",
                                 "tcp://localhost:5003");
        bstar_voter (self->bstar, "tcp://*:5566", ZMQ_ROUTER,
                     s_send_snapshot, self);
        self->port = 5566;
        self->peer = 5556;
        self->primary = FALSE;
    }
    else {
        printf ("Usage: clonesrv4 { -p | -b }\n");
        free (self);
        exit (0);
    }
    //  Primary server will become first master
    if (self->primary)
        self->kvmap = zhash_new ();

    self->ctx = zctx_new ();
    self->pending = zlist_new ();
    bstar_set_verbose (self->bstar, FALSE);

    //  Set up our clone server sockets
    self->publisher = zsocket_new (self->ctx, ZMQ_PUB);
    self->collector = zsocket_new (self->ctx, ZMQ_SUB);
    zsocket_bind (self->publisher, "tcp://*:%d", self->port + 1);
    zsocket_bind (self->collector, "tcp://*:%d", self->port + 2);

    //  Set up our own clone client interface to peer
    self->subscriber = zsocket_new (self->ctx, ZMQ_SUB);
    zsocket_connect (self->subscriber, "tcp://localhost:%d", self->peer + 1);

    //  Register state change handlers
    bstar_new_master (self->bstar, s_new_master, self);
    bstar_new_slave (self->bstar, s_new_slave, self);

    //  Register our other handlers with the bstar reactor
    zloop_reader (bstar_zloop (self->bstar), self->collector, s_collector, self);
    zloop_timer (bstar_zloop (self->bstar), 1000, 0, s_send_hugz, self);

    //  Start the Bstar reactor
    bstar_start (self->bstar);

    //  Interrupted, so shut down
    while (zlist_size (self->pending)) {
        kvmsg_t *kvmsg = (kvmsg_t *) zlist_pop (self->pending);
        kvmsg_destroy (&kvmsg);
    }
    zlist_destroy (&self->pending);
    bstar_destroy (&self->bstar);
    zhash_destroy (&self->kvmap);
    zctx_destroy (&self->ctx);
    free (self);

    return 0;
}


//  ---------------------------------------------------------------------
//  Send snapshots to clients who ask for them

static int s_send_single (char *key, void *data, void *args);

//  Routing information for a KV snapshot
typedef struct {
    void *socket;           //  ROUTER socket to send to
    zframe_t *identity;     //  Identity of peer who requested state
} kvroute_t;

static int
s_send_snapshot (zloop_t *loop, void *socket, void *args)
{
    clonesrv_t *self = (clonesrv_t *) args;

    zframe_t *identity = zframe_recv (socket);
    zframe_t *request = zframe_recv (socket);
    if (identity && request) {
        assert (zframe_streq (request, "ICANHAZ?"));
        zframe_destroy (&request);

        //  Send state socket to client
        kvroute_t routing = { socket, identity };
        zhash_foreach (self->kvmap, s_send_single, &routing);

        //  Now send END message with sequence number
        zclock_log ("I: sending shapshot=%d", (int) self->sequence);
        zframe_send (&identity, socket, ZFRAME_MORE);
        kvmsg_t *kvmsg = kvmsg_new (self->sequence);
        kvmsg_set_key  (kvmsg, "KTHXBAI");
        kvmsg_set_body (kvmsg, (byte *) "", 0);
        kvmsg_send     (kvmsg, socket);
        kvmsg_destroy (&kvmsg);
    }
    return 0;
}


//  Send one state snapshot key-value pair to a socket
//  Hash item data is our kvmsg object, ready to send
static int
s_send_single (char *key, void *data, void *args)
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
//  Collect updates from clients
//  If we're master, we apply these to the kvmap
//  If we're slave, or unsure, we queue them on our pending list

static int s_was_pending (clonesrv_t *self, kvmsg_t *kvmsg);

static int
s_collector (zloop_t *loop, void *socket, void *args)
{
    clonesrv_t *self = (clonesrv_t *) args;

    kvmsg_t *kvmsg = kvmsg_recv (socket);
    if (kvmsg) {
        if (self->master) {
            kvmsg_set_sequence (kvmsg, ++self->sequence);
            kvmsg_send (kvmsg, self->publisher);
            kvmsg_store (&kvmsg, self->kvmap);
            zclock_log ("I: publishing update=%d", (int) self->sequence);
        }
        else {
            //  If we already got message from master, drop it
            if (s_was_pending (self, kvmsg))
                kvmsg_destroy (&kvmsg);
            else
                //  Else hold on pending list
                zlist_append (self->pending, kvmsg);
        }
    }
    return 0;
}

//  If message was already on pending list, remove it and
//  return TRUE, else return FALSE.

static int
s_was_pending (clonesrv_t *self, kvmsg_t *kvmsg)
{
    kvmsg_t *held = (kvmsg_t *) zlist_first (self->pending);
    while (held) {
        if (memcmp (kvmsg_uuid (kvmsg),
                    kvmsg_uuid (held), sizeof (uuid_t)) == 0) {
            zlist_remove (self->pending, held);
            return TRUE;
        }
        held = (kvmsg_t *) zlist_next (self->pending);
    }
    return FALSE;
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
//  State change handlers
//  We're becoming master
//
//  The backup server applies its pending list to its own hash table,
//  and then starts to process state snapshot requests.

static int
s_new_master (zloop_t *loop, void *unused, void *args)
{
    clonesrv_t *self = (clonesrv_t *) args;

    self->master = TRUE;
    self->slave = FALSE;
    zloop_cancel (bstar_zloop (self->bstar), self->subscriber);

    //  Apply pending list to own hash table
    while (zlist_size (self->pending)) {
        kvmsg_t *kvmsg = (kvmsg_t *) zlist_pop (self->pending);
        kvmsg_set_sequence (kvmsg, ++self->sequence);
        kvmsg_send (kvmsg, self->publisher);
        kvmsg_store (&kvmsg, self->kvmap);
        zclock_log ("I: publishing pending=%d", (int) self->sequence);
    }
    return 0;
}

//  ---------------------------------------------------------------------
//  We're becoming slave

static int
s_new_slave (zloop_t *loop, void *unused, void *args)
{
    clonesrv_t *self = (clonesrv_t *) args;

    zhash_destroy (&self->kvmap);
    self->master = FALSE;
    self->slave = TRUE;
    zloop_reader (bstar_zloop (self->bstar), self->subscriber,
                  s_subscriber, self);

    return 0;
}

//  ---------------------------------------------------------------------
//  Collect updates from peer (master)
//  We're always slave when we get these updates

static int
s_subscriber (zloop_t *loop, void *socket, void *args)
{
    clonesrv_t *self = (clonesrv_t *) args;
    //  Get state snapshot if necessary
    if (self->kvmap == NULL) {
        self->kvmap = zhash_new ();
        void *snapshot = zsocket_new (self->ctx, ZMQ_DEALER);
        zsocket_connect (snapshot, "tcp://localhost:%d", self->peer);
        zclock_log ("I: asking for snapshot from: tcp://localhost:%d",
                    self->peer);
        zstr_send (snapshot, "ICANHAZ?");
        while (TRUE) {
            kvmsg_t *kvmsg = kvmsg_recv (snapshot);
            if (!kvmsg)
                break;          //  Interrupted
            if (streq (kvmsg_key (kvmsg), "KTHXBAI")) {
                self->sequence = kvmsg_sequence (kvmsg);
                kvmsg_destroy (&kvmsg);
                break;          //  Done
            }
            kvmsg_store (&kvmsg, self->kvmap);
        }
        zclock_log ("I: received snapshot=%d", (int) self->sequence);
        zsocket_destroy (self->ctx, snapshot);
    }
    //  Find and remove update off pending list
    kvmsg_t *kvmsg = kvmsg_recv (socket);
    if (!kvmsg)
        return 0;

    if (strneq (kvmsg_key (kvmsg), "HUGZ")) {
        if (!s_was_pending (self, kvmsg)) {
            //  If master update came before client update, flip it
            //  around, store master update (with sequence) on pending
            //  list and use to clear client update when it comes later
            zlist_append (self->pending, kvmsg_dup (kvmsg));
        }
        //  If update is more recent than our kvmap, apply it
        if (kvmsg_sequence (kvmsg) > self->sequence) {
            self->sequence = kvmsg_sequence (kvmsg);
            kvmsg_store (&kvmsg, self->kvmap);
            zclock_log ("I: received update=%d", (int) self->sequence);
        }
        else
            kvmsg_destroy (&kvmsg);
    }
    else
        kvmsg_destroy (&kvmsg);

    return 0;
}
