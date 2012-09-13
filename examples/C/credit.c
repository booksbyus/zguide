//
//  Credit based flow control example
//

#include "czmq.h"

//  This is the size of our end-to-end pipeline, in bytes
#define PIPELINE_SIZE   1024 * 1024

//  We'll aim to keep the pipeline 50-75% full at all times
#define CREDIT_SLICE    PIPELINE_SIZE / 4

//  Here we just want to prove that we never exceed the HWM,
//  so we can send chunks of a fixed size
#define CHUNK_SIZE      65536
#define SERVER_HWM      PIPELINE_SIZE / CHUNK_SIZE

//  .split Client thread
//  We start by sending the server enough credit to fill the whole
//  pipeline. Then as we receive messages we top-up the credit in
//  slices:

static void
client_thread (void *args, zctx_t *ctx, void *pipe)
{
    void *dealer = zsocket_new (ctx, ZMQ_DEALER);
    zsocket_connect (dealer, "tcp://127.0.0.1:10001");

    //  Start by asking the server to fill the pipeline
    zstr_sendf (dealer, "%ld", PIPELINE_SIZE);

    int expected_seq = 0;   //  Check we don't lose chunks
    int uncredited = 0;     //  Data received but not credited yet

    while (true) {
        zmsg_t *msg = zmsg_recv (dealer);
        if (!msg)
            break;          //  Interrupted

        //  Message has two frames, sequence number and body
        char *sequence = zmsg_popstr (msg);
        int current_seq = atoi (sequence);
        free (sequence);
        zframe_t *content = zmsg_pop (msg);
        assert (content);
        if (current_seq != expected_seq) {
            printf ("E: server dropped %d messages, exit (%d/%d)\n",
                (current_seq - expected_seq), current_seq, expected_seq);
            exit (1);
        }
        expected_seq++;

        //  Count uncredited data, send top-up credit if needed
        uncredited += zframe_size (content);
        if (uncredited > CREDIT_SLICE) {
            uncredited -= CREDIT_SLICE;
            zstr_sendf (dealer, "%ld", CREDIT_SLICE);
        }
        zframe_destroy (&content);
        zmsg_destroy (&msg);

        //  Sleep for some random interval up to 10 msecs
        zclock_sleep (randof (10));
    }
}


//  .split Server thread
//  We track the credit per connected client:

typedef struct {
    zframe_t *identity;
    int credit;
    int sequence;
} client_t;

static void
server_thread (void *args, zctx_t *ctx, void *pipe)
{
    void *router = zsocket_new (ctx, ZMQ_ROUTER);
    zsocket_set_hwm (router, SERVER_HWM);
    zsocket_bind (router, "tcp://*:10001");

    //  We'll hold the clients on a simple list
    zlist_t *clients = zlist_new ();

    //  We're purely driven by input events
    while (true) {
        zmsg_t *msg = zmsg_recv (router);
        if (!msg)
            break;          //  Interrupted

        //  Only message from clients is credit
        zframe_t *client_frame = zmsg_pop (msg);
        char *credit_string = zmsg_popstr (msg);
        int credit = atoi (credit_string);
        free (credit_string);

        //  Look for existing client with this identity
        client_t *client = (client_t *) zlist_first (clients);
        while (client) {
            if (zframe_eq (client->identity, client_frame))
                break;
            client = (client_t *) zlist_next (clients);
        }
        //  If this client is new, create an object and save it
        if (client == NULL) {
            client = (client_t *) zmalloc (sizeof (client_t));
            client->identity = client_frame;
            zlist_append (clients, client);
        }
        else
            zframe_destroy (&client_frame);

        //  Accumulate credit for this client
        client->credit += credit;
        zmsg_destroy (&msg);

        //  We now stream data to all clients with available credit
        //  until their credit is used up. We then wait for clients
        //  to send us new credit.
        client = (client_t *) zlist_first (clients);
        while (client) {
            while (client->credit >= CHUNK_SIZE) {
                //  Send chunk of data to the client
                zframe_t *content = zframe_new (NULL, CHUNK_SIZE);
                zframe_send (&client->identity, router,
                             ZFRAME_MORE + ZFRAME_REUSE);
                zstr_sendfm (router, "%d", client->sequence++);
                zframe_send (&content, router, 0);
                //  Discount credit
                client->credit -= CHUNK_SIZE;
            }
            client = (client_t *) zlist_next (clients);
        }
    }
}

//  .split File main thread
//  The main task starts the client and server threads; it's easier
//  to test this as a single process with threads, than as multiple
//  processes:

int main (void)
{
    zctx_t *ctx = zctx_new ();
    printf ("I: starting server...\n");
    zthread_fork (ctx, server_thread, NULL);
    printf ("I: starting clients...\n");
    int clients = 10;
    while (clients--)
        zthread_fork (ctx, client_thread, NULL);

    //  Wait until the user presses Ctrl-C
    while (!zctx_interrupted)
        sleep (1);

    zctx_destroy (&ctx);
    return 0;
}
