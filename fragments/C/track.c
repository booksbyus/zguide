#include "czmq.h"
#include "../include/fmq.h"

int main (int argc, char *argv [])
{
    if (argc < 3) {
        puts ("usage: track original-directory tracking-directory");
        return 0;
    }
    fmq_server_t *server = fmq_server_new ();
    fmq_server_configure (server, "anonymous.cfg");
    fmq_server_publish (server, argv [1], "/");
    fmq_server_set_anonymous (server, true);
    fmq_server_bind (server, "tcp://*:6000");

    fmq_client_t *client = fmq_client_new ();
    fmq_client_connect   (client, "tcp://localhost:6000");
    fmq_client_set_inbox (client, argv [2]);
    fmq_client_subscribe (client, "/");

    while (!zctx_interrupted)
        sleep (1);
    puts ("interrupted");

    fmq_server_destroy (&server);
    fmq_client_destroy (&client);
    return 0;
}
