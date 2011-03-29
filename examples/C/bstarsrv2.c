//
//  Binary Star server
//
#include "bstar.h"

int main (int argc, char *argv [])
{
    //  Arguments can be either of:
    //      -p  primary server, at tcp://localhost:5001
    //      -b  backup server, at tcp://localhost:5002
    bstar_t *bstar = bstar_new (ZMQ_ROUTER);
    if (argc == 2 && streq (argv [1], "-p")) {
        printf ("I: Primary master, waiting for backup (slave)\n");
        bstar_peer (bstar, "tcp://*:5003", "tcp://localhost:5004");
        bstar_init (bstar, "tcp://*:5001", TRUE);
    }
    else
    if (argc == 2 && streq (argv [1], "-b")) {
        printf ("I: Backup slave, waiting for primary (master)\n");
        bstar_peer (bstar, "tcp://*:5004", "tcp://localhost:5003");
        bstar_init (bstar, "tcp://*:5002", FALSE);
    }
    else {
        printf ("Usage: bstarsrv { -p | -b }\n");
        exit (0);
    }
    while (!s_interrupted) {
        zmsg_t *msg = bstar_recv (bstar);
        if (msg)    //  Answer client by echoing request back
            bstar_send (bstar, &msg);
        else
            break;
    }
    if (s_interrupted)
        printf ("W: interrupted\n");

    bstar_destroy (&bstar);
    return 0;
}
