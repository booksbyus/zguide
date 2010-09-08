//
//  Custom router using XREP to XREP
//
#include "zhelpers.h"

//  We have two workers, here we copy the code, normally these would
//  run on different boxes...
//
void *worker_a (void *context) {
    void *worker = zmq_socket (context, ZMQ_REP);
    zmq_setsockopt (worker, ZMQ_IDENTITY, "A", 1);
    zmq_connect (worker, "ipc://routing");

    int total = 0;
    while (1) {
        //  We receive one part, with the workload
        char *request = s_recv (worker);
        int finished = (strcmp (request, "END") == 0);
        free (request);
        //  We must reply, since this is a synchronous REP socket
        s_send (worker, "This is the reply from A");
        if (finished) {
            printf ("A received: %d\n", total);
            break;
        }
        total++;
    }
    return (NULL);
}

void *worker_b (void *context) {
    void *worker = zmq_socket (context, ZMQ_REP);
    zmq_setsockopt (worker, ZMQ_IDENTITY, "B", 1);
    zmq_connect (worker, "ipc://routing");

    int total = 0;
    while (1) {
        //  We receive one part, with the request workload
        char *request = s_recv (worker);
        int finished = (strcmp (request, "END") == 0);
        free (request);
        //  We must reply, since this is a synchronous REP socket
        s_send (worker, "This is the reply from B");
        if (finished) {
            printf ("B received: %d\n", total);
            break;
        }
        total++;
    }
    return (NULL);
}

#define within(num) (int) ((float) num * random () / (RAND_MAX + 1.0))

int main () {
    void *context = zmq_init (1);

    void *client = zmq_socket (context, ZMQ_XREP);
    zmq_bind (client, "ipc://routing");

    pthread_t worker;
    pthread_create (&worker, NULL, worker_a, context);
    pthread_create (&worker, NULL, worker_b, context);

    //  Wait for threads to stabilize
    sleep (1);

    //  Send 100K tasks scattered to A twice as often as B
    int task_nbr;
    srandom ((unsigned) time (NULL));
    for (task_nbr = 0; task_nbr < 10; task_nbr++) {
        //  Send three message parts, first the address...
        if (within (3) > 0)
            s_sendmore (client, "A");
        else
            s_sendmore (client, "B");
        //  Then a null message part which REP expects
        s_sendmore (client, "");
        //  And then the workload
        s_send (client, "This is the workload");

        //  Now we'll wait for the reply which will come
        //  in three parts, [address] [empty] [data]
        char *address = s_recv (client);
        printf ("[%s]\n", address);
        free (address);
        char *empty = s_recv (client);
        printf ("[%s]\n", empty);
        free (empty);
        char *reply = s_recv (client);
        printf ("[%s]\n", reply);
        free (reply);
    }
    s_sendmore (client, "A");
    s_sendmore (client, "");
    s_send     (client, "END");

    s_sendmore (client, "B");
    s_sendmore (client, "");
    s_send     (client, "END");

    sleep (1);              //  Give 0MQ/2.0.x time to flush output
    return 0;
}
