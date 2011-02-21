//
//  Custom routing Router to Dealer (XREP to XREQ)
//
//  Changes for 2.1:
//  - added version assertion
//  - use separate contexts for each thread
//  - close sockets in each child thread
//  - call zmq_term in each thread before ending
//  - removed sleep(1) at end of main thread
//
#include "zhelpers.h"

//  We have two workers, here we copy the code, normally these would
//  run on different boxes...
//
void *worker_a (void *args) {
    void *context = zmq_init (1);
    void *worker = zmq_socket (context, ZMQ_XREQ);
    zmq_setsockopt (worker, ZMQ_IDENTITY, "A", 1);
    zmq_connect (worker, "ipc://routing.ipc");

    int total = 0;
    while (1) {
        //  We receive one part, with the workload
        char *request = s_recv (worker);
        int finished = (strcmp (request, "END") == 0);
        free (request);
        if (finished) {
            printf ("A received: %d\n", total);
            break;
        }
        total++;
    }
    zmq_close (worker);
    zmq_term (context);
    return (NULL);
}

void *worker_b (void *args) {
    void *context = zmq_init (1);
    void *worker = zmq_socket (context, ZMQ_XREQ);
    zmq_setsockopt (worker, ZMQ_IDENTITY, "B", 1);
    zmq_connect (worker, "ipc://routing.ipc");

    int total = 0;
    while (1) {
        //  We receive one part, with the workload
        char *request = s_recv (worker);
        int finished = (strcmp (request, "END") == 0);
        free (request);
        if (finished) {
            printf ("B received: %d\n", total);
            break;
        }
        total++;
    }
    zmq_close (worker);
    zmq_term (context);
    return (NULL);
}

int main () {
    s_version_assert (2, 1);
    void *context = zmq_init (1);

    void *client = zmq_socket (context, ZMQ_XREP);
    zmq_bind (client, "ipc://routing.ipc");

    pthread_t worker;
    pthread_create (&worker, NULL, worker_a, NULL);
    pthread_create (&worker, NULL, worker_b, NULL);

    //  Wait for threads to connect, since otherwise the messages
    //  we send won't be routable.
    sleep (1);

    //  Send 10 tasks scattered to A twice as often as B
    int task_nbr;
    srandom ((unsigned) time (NULL));
    for (task_nbr = 0; task_nbr < 10; task_nbr++) {
        //  Send two message parts, first the address...
        if (randof (3) > 0)
            s_sendmore (client, "A");
        else
            s_sendmore (client, "B");

        //  And then the workload
        s_send (client, "This is the workload");
    }
    s_sendmore (client, "A");
    s_send     (client, "END");

    s_sendmore (client, "B");
    s_send     (client, "END");

    zmq_close (client);
    zmq_term (context);
    return 0;
}
