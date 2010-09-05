//
//  Task sink in C
//  Binds PULL socket to tcp://localhost:5558
//  Collects results from workers via that socket
//
#include "zhelpers.h"

int main (int argc, char *argv[])
{
    //  Prepare our context and socket
    void *context = zmq_init (1);
    void *receiver = zmq_socket (context, ZMQ_PULL);
    zmq_bind (receiver, "tcp://*:5558");

    //  Wait for start of batch
    char *string = s_recv (receiver);
    free (string);

    //  Start our clock now
    struct timeval tstart;
    gettimeofday (&tstart, NULL);

    //  Process 100 confirmations
    int task_nbr;
    int total_msec = 0;     //  Total calculated cost in msecs
    for (task_nbr = 0; task_nbr < 100; task_nbr++) {
        char *string = s_recv (receiver);
        free (string);
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
    return 0;
}
