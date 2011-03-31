//
//  Binary Star server
//

//  Lets us 'build bstarsrv2' and 'build all'
#include "bstar.c"

int main (int argc, char *argv [])
{
    //  Arguments can be either of:
    //      -p  primary server, at tcp://localhost:5001
    //      -b  backup server, at tcp://localhost:5002
    bstar_t *bstar;
    if (argc == 2 && streq (argv [1], "-p")) {
        printf ("I: Primary master, waiting for backup (slave)\n");
        bstar = bstar_new (BSTAR_PRIMARY, 
            "tcp://*:5003", "tcp://localhost:5004");
        bstar_listen (bstar, "tcp://*:5001", ZMQ_ROUTER);
    }
    else
    if (argc == 2 && streq (argv [1], "-b")) {
        printf ("I: Backup slave, waiting for primary (master)\n");
        bstar = bstar_new (BSTAR_BACKUP,
            "tcp://*:5004", "tcp://localhost:5003");
        bstar_listen (bstar, "tcp://*:5002", ZMQ_ROUTER);
    }
    else {
        printf ("Usage: bstarsrv { -p | -b }\n");
        exit (0);
    }
    //  Now handle activity from clients
    while (!s_interrupted) {
        void *socket = bstar_wait (bstar);
        if (!socket)
            break;              //  Interrupted, or fatal error

        //  We're just an echo server
        zmsg_t *msg = zmsg_recv (socket);
        if (msg)
            zmsg_send (&msg, socket);
    }
    bstar_destroy (&bstar);
    return 0;
}
