//
//  Credit based flow control example
//
//  We start some clients that talk to a single server via a DEALER
//  to ROUTER setup. Clients say hello to the server and then start
//  to receive random data. The server sends as fast as it can, but
//  only within credit window created by client.
//
#include "czmq.h"

#define NBR_CLIENTS     10

//  The TRANSIT_TOTAL size defines the total data in transit,
//  covering 0MQ send and recv queues, TCP send and recv buffers,
//  and packets in flight on the network. The client starts by
//  sending TRANSIT_TOTAL credit to the server, and thereafter
//  sends TRANSIT_SLICE credit after receiving TRANSIT_SLICE bytes.

#define TRANSIT_TOTAL   1024 * 1024
#define TRANSIT_SLICE   TRANSIT_TOTAL / 4

//  We assert that the flow-control mechanism works by setting a
//  HWM on the server send queue, and sequencing messages. If the
//  queue hits HWM for any client, 0MQ will drop messages, as this
//  is the exception strategy for ROUTER sockets. The client can
//  detect this and abort.
//
//  Difficulty: 0MQ counts messages, not bytes. So HWM is not an
//  accurate measure. To solve this we batch small messages, and
//  fragment larger messages into blocks of FRAGMENT_SIZE octets.
//
//  For the example we simply generate FRAGMENT_SIZE messages.
//  In a more cynical test we would batch and fragment on sending.
//  But, once flow control works, we don't need the HWM at all.

#define FRAGMENT_SIZE   65536

//  Knowing the TRANSIT_TOTAL and the FRAGMENT_SIZE, we can set the
//  HWM to be (TRANSIT_TOTAL / FRAGMENT_SIZE).

#define SERVER_HWM      TRANSIT_TOTAL / FRAGMENT_SIZE


//  -------------------------------------------------------------------
//  Client task

static void *
client_task (void *args)
{
    zctx_t *ctx = zctx_new ();
    void *dealer = zsocket_new (ctx, ZMQ_DEALER);
    zsocket_connect (dealer, "tcp://127.0.0.1:10001");

    //  Start by sending TRANSIT_TOTAL credit to server
    zstr_sendf (dealer, "%ld", TRANSIT_TOTAL);

    //  Now consume and verify incoming messages and refresh
    //  credit asynchronously as needed
    int expected_seq = 0;
    int received = 0;

    while (TRUE) {
        zmsg_t *msg = zmsg_recv (dealer);
        if (!msg)
            break;

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

        //  Count received data, send top-up credit if needed
        received += zframe_size (content);
        if (received > TRANSIT_SLICE) {
            received -= TRANSIT_SLICE;
            zstr_sendf (dealer, "%ld", TRANSIT_SLICE);
        }
        zframe_destroy (&content);
        zmsg_destroy (&msg);

        //  Sleep for some random interval up to 100 msecs
        zclock_sleep (randof (10));
    }
    zctx_destroy (&ctx);
    return NULL;
}


//  -------------------------------------------------------------------
//  Server task

//  Clients are represented by this data structure
typedef struct {
    zframe_t *identity;
    int credit;
    int sequence;
} client_t;

static void *
server_task (void *args)
{
    zctx_t *ctx = zctx_new ();
    void *router = zsocket_new (ctx, ZMQ_ROUTER);
    zsocket_set_hwm (router, SERVER_HWM + 10);
    zsocket_bind (router, "tcp://*:10001");

    //  We'll hold the clients on a simple list
    zlist_t *clients = zlist_new ();

    //  We're purely driven by input events
    while (1) {
        zmsg_t *msg = zmsg_recv (router);
        if (!msg)
            break;

        //  PROCESS CLIENT CREDITS
        //  -------------------------------------------------------
        //  Only message we accept from clients is a credit message

        zframe_t *client_frame = zmsg_pop (msg);
        char *credit_string = zmsg_popstr (msg);
        int credit = atoi (credit_string);
        free (credit_string);

        //  Look for pre-existing client with this identity
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

        //  DISPATCH TO CLIENTS
        //  -------------------------------------------------------
        //  We now stream data to all clients with available credit
        //  until their credit is used up. We then wait for clients
        //  to send us new credit.

        //  Process entire client list in turn
        client = (client_t *) zlist_first (clients);
        while (client) {
            while (client->credit >= FRAGMENT_SIZE) {
                int msgsize = FRAGMENT_SIZE + randof (1000) - randof (1000);

                //  Send fragment of data to the client
                zframe_t *content = zframe_new (NULL, msgsize);
                zframe_send (&client->identity, router,
                             ZFRAME_MORE + ZFRAME_REUSE);
                zstr_sendfm (router, "%d", client->sequence++);
                zframe_send (&content, router, 0);

                //  Discount credit
                client->credit -= msgsize;
            }
            client = (client_t *) zlist_next (clients);
        }
    }
    zctx_destroy (&ctx);
    return NULL;
}

int main (void)
{
    //  Create threads
    zctx_t *ctx = zctx_new ();
    printf ("I: starting server...\n");
    zthread_new (server_task, NULL);

    printf ("I: starting %d clients...\n", NBR_CLIENTS);
    int client_nbr;
    for (client_nbr = 0; client_nbr < NBR_CLIENTS; client_nbr++)
        zthread_new (client_task, NULL);

    while (!zctx_interrupted)
        sleep (1);

    zctx_destroy (&ctx);
    return 0;
}
