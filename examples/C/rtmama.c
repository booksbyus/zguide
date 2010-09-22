//
//  Custom routing Router to Mama (XREP to REQ)
//
#include "zhelpers.h"

#define NBR_WORKERS 10

static void *
worker_thread (void *context) {
    void *worker = zmq_socket (context, ZMQ_REQ);

    //  We use a string identity for ease here
    s_set_id (worker);
    zmq_connect (worker, "ipc://routing.ipc");

    int total = 0;
    while (1) {
        //  Tell the router we're ready for work
        s_send (worker, "ready");

        //  Get workload from router, until finished
        char *workload = s_recv (worker);
        int finished = (strcmp (workload, "END") == 0);
        free (workload);
        if (finished) {
            printf ("Processed: %d tasks\n", total);
            break;
        }
        total++;

        //  Do some random work
        struct timespec t;
        t.tv_sec = 0;
        t.tv_nsec = within (100000000) + 1;
        nanosleep (&t, NULL);
    }
    return (NULL);
}

int main () {
    void *context = zmq_init (1);
    void *client = zmq_socket (context, ZMQ_XREP);
    zmq_bind (client, "ipc://routing.ipc");
    srandom ((unsigned) time (NULL));

    int worker_nbr;
    for (worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++) {
        pthread_t worker;
        pthread_create (&worker, NULL, worker_thread, context);
    }
    int task_nbr;
    for (task_nbr = 0; task_nbr < NBR_WORKERS * 10; task_nbr++) {
        //  LRU worker is next waiting in queue
        char *address = s_recv (client);
        char *empty = s_recv (client);
        free (empty);
        char *ready = s_recv (client);
        free (ready);

        s_sendmore (client, address);
        s_sendmore (client, "");
        s_send (client, "This is the workload");
        free (address);
    }
    //  Now ask mamas to shut down and report their results
    for (worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++) {
        char *address = s_recv (client);
        char *empty = s_recv (client);
        free (empty);
        char *ready = s_recv (client);
        free (ready);

        s_sendmore (client, address);
        s_sendmore (client, "");
        s_send (client, "END");
        free (address);
    }
    sleep (1);              //  Give 0MQ/2.0.x time to flush output
    zmq_term (context);
    return 0;
}
