//
//  Paranoid Pirate queue
//
#include "zmsg.h"

#define MAX_WORKERS         100
#define HEARTBEAT_LIVENESS  3       //  3-5 is reasonable
#define HEARTBEAT_INTERVAL  1000    //  msecs

//  This defines one active worker in our worker queue

typedef struct {
    char *identity;             //  Address of worker
    int64_t expiry;             //  Expires at this time
} worker_t;

typedef struct {
    size_t size;                //  Number of workers
    worker_t workers [MAX_WORKERS];
} queue_t;

//  Dequeue operation for queue implemented as array of anything
#define DEQUEUE(queue, index) memmove (      \
    &(queue) [index], &(queue) [index + 1],  \
    (sizeof (queue) / sizeof (*queue) - index) * sizeof (queue [0]))

//  Insert worker at end of queue, reset expiry
//  Worker must not already be in queue
static void
s_worker_append (queue_t *queue, char *identity)
{
    int index;
    for (index = 0; index < queue->size; index++)
        if (streq (queue->workers [index].identity, identity))
            break;

    if (index < queue->size)
        printf ("E: duplicate worker identity %s", identity);
    else {
        assert (queue->size < MAX_WORKERS);
        queue->workers [queue->size].identity = identity;
        queue->workers [queue->size].expiry = s_clock ()
            + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS;
        queue->size++;
    }
}

//  Remove worker from queue, if present
static void
s_worker_delete (queue_t *queue, char *identity)
{
    int index;
    for (index = 0; index < queue->size; index++)
        if (streq (queue->workers [index].identity, identity))
            break;

    if (index < queue->size) {
        free (queue->workers [index].identity);
        DEQUEUE (queue->workers, index);
        queue->size--;
    }
}

//  Reset worker expiry, worker must be present
static void
s_worker_refresh (queue_t *queue, char *identity)
{
    int index;
    for (index = 0; index < queue->size; index++)
        if (streq (queue->workers [index].identity, identity))
            break;

    if (index < queue->size)
        queue->workers [index].expiry = s_clock ()
            + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS;
    else
        printf ("E: worker %s not ready\n", identity);
}

//  Pop next available worker off queue, return identity
static char *
s_worker_dequeue (queue_t *queue)
{
    assert (queue->size);
    char *identity = queue->workers [0].identity;
    DEQUEUE (queue->workers, 0);
    queue->size--;
    return identity;
}

//  Look for & kill expired workers
static void
s_queue_purge (queue_t *queue)
{
    //  Work backwards from oldest so we don't do useless work
    int index;
    for (index = queue->size - 1; index >= 0; index--) {
        if (s_clock () > queue->workers [index].expiry) {
            free (queue->workers [index].identity);
            DEQUEUE (queue->workers, index);
            queue->size--;
            index--;
        }
    }
}

int main (void)
{
    //  Prepare our context and sockets
    void *context = zmq_init (1);
    void *frontend = zmq_socket (context, ZMQ_ROUTER);
    void *backend  = zmq_socket (context, ZMQ_ROUTER);
    zmq_bind (frontend, "tcp://*:5555");    //  For clients
    zmq_bind (backend,  "tcp://*:5556");    //  For workers

    //  Queue of available workers
    queue_t *queue = (queue_t *) calloc (1, sizeof (queue_t));

    //  Send out heartbeats at regular intervals
    uint64_t heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;

    while (1) {
        zmq_pollitem_t items [] = {
            { backend,  0, ZMQ_POLLIN, 0 },
            { frontend, 0, ZMQ_POLLIN, 0 }
        };
        //  Poll frontend only if we have available workers
        if (queue->size)
            zmq_poll (items, 2, HEARTBEAT_INTERVAL * 1000);
        else
            zmq_poll (items, 1, HEARTBEAT_INTERVAL * 1000);

        //  Handle worker activity on backend
        if (items [0].revents & ZMQ_POLLIN) {
            zmsg_t *msg = zmsg_recv (backend);
            char *identity = zmsg_unwrap (msg);

            //  Return reply to client if it's not a control message
            if (zmsg_parts (msg) == 1) {
                if (streq (zmsg_address (msg), "READY")) {
                    s_worker_delete (queue, identity);
                    s_worker_append (queue, identity);
                }
                else
                if (streq (zmsg_address (msg), "HEARTBEAT"))
                    s_worker_refresh (queue, identity);
                else {
                    printf ("E: invalid message from %s\n", identity);
                    zmsg_dump (msg);
                    free (identity);
                }
                zmsg_destroy (&msg);
            }
            else {
                zmsg_send (&msg, frontend);
                s_worker_append (queue, identity);
            }
        }
        if (items [1].revents & ZMQ_POLLIN) {
            //  Now get next client request, route to next worker
            zmsg_t *msg = zmsg_recv (frontend);
            char *identity = s_worker_dequeue (queue);
            zmsg_push (msg, identity);
            zmsg_send (&msg, backend);
            free (identity);
        }

        //  Send heartbeats to idle workers if it's time
        if (s_clock () > heartbeat_at) {
            int index;
            for (index = 0; index < queue->size; index++) {
                zmsg_t *msg = zmsg_new ("HEARTBEAT");
                zmsg_wrap (msg, queue->workers [index].identity, NULL);
                zmsg_send (&msg, backend);
            }
            heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
        }
        s_queue_purge (queue);
    }
    //  We never exit the main loop
    //  But pretend to do the right shutdown anyhow
    while (queue->size)
        free (s_worker_dequeue (queue));
    free (queue);
    zmq_close (frontend);
    zmq_close (backend);
    return 0;
}
