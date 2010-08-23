//
//  Task sink in C - design 2
//  Adds pub-sub flow to send kill signal to workers
//
#include <zmq.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>

int main (int argc, char *argv[])
{
    void *context = zmq_init (1);

    //  Socket to receive messages on
    void *receiver = zmq_socket (context, ZMQ_PULL);
    zmq_bind (receiver, "tcp://*:5558");

    //  Socket for worker control
    void *controller = zmq_socket (context, ZMQ_PUB);
    zmq_bind (controller, "tcp://*:5559");

    //  Wait for start of batch
    zmq_msg_t message;
    zmq_msg_init (&message);
    zmq_recv (receiver, &message, 0);
    zmq_msg_close (&message);

    //  Start our clock now
    struct timeval tstart;
    gettimeofday (&tstart, NULL);

    //  Process 100 confirmations
    int task_nbr;
    int total_msec = 0;     //  Total calculated cost in msecs
    for (task_nbr = 0; task_nbr < 100; task_nbr++) {
        zmq_msg_init (&message);
        zmq_recv (receiver, &message, 0);
        zmq_msg_close (&message);
        if ((task_nbr / 10) * 10 == task_nbr)
            printf (":");
        else
            printf (".");
        fflush (stdout);
    }
    //  Calculate and report duration of batch
    struct timeval tend, tdiff;
    gettimeofday (&tend, NULL);

    if (tend.tv_usec < tstart.tv_usec) {
        tdiff.tv_sec = tend.tv_sec - tstart.tv_sec - 1;
        tdiff.tv_usec = 1000000 + tend.tv_usec - tstart.tv_usec;
    }
    else {
        tdiff.tv_sec = tend.tv_sec - tstart.tv_sec;
        tdiff.tv_usec = tend.tv_usec - tstart.tv_usec;
    }
    total_msec = tdiff.tv_sec * 1000 + tdiff.tv_usec / 1000;
    printf ("Total elapsed time: %d msec\n", total_msec);

    //  Send kill signal to workers
    zmq_msg_init_data (&message, "KILL", 5, NULL, NULL);
    zmq_send (controller, &message, 0);
    zmq_msg_close (&message);

    //  Finished
    sleep (1);              //  Give 0MQ time to deliver
    return 0;
}
