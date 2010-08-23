//
//  Weather update client in C
//  Connects SUB socket to tcp://localhost:5556
//  Collects weather updates and finds avg temp in zipcode
//
#include <zmq.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

int main (int argc, char *argv[])
{
    void *context = zmq_init (1);

    //  Socket to talk to server
    printf ("Collecting updates from weather server...\n");
    void *subscriber = zmq_socket (context, ZMQ_SUB);
    zmq_connect (subscriber, "tcp://localhost:5556");

    //  Subscribe to zipcode, default is NYC, 10001
    char *filter = (argc > 1)? argv [1]: "10001 ";
    zmq_setsockopt (subscriber, ZMQ_SUBSCRIBE, filter, strlen (filter));

    //  Process 100 updates
    int update_nbr;
    long total_temp = 0;
    for (update_nbr = 0; update_nbr < 100; update_nbr++) {
        zmq_msg_t update;
        int zipcode, temperature, relhumidity;

        zmq_msg_init (&update);
        zmq_recv (subscriber, &update, 0);
        sscanf ((char *) zmq_msg_data (&update), "%d %d %d",
            &zipcode, &temperature, &relhumidity);
        total_temp += temperature;
        zmq_msg_close (&update);
    }
    printf ("Average temperature for zipcode '%s' was %dF\n",
        filter, (int) (total_temp / update_nbr));
    return 0;
}
