#include "czmq.h"
#include "nom_server.h"

int main (int argc, char *argv [])
{
    printf ("Starting NOM protocol server on port 6000...\n");
    nom_server_t *server = nom_server_new ();
    nom_server_bind (server, "tcp://*:6000");
    nom_server_wait (server);
    nom_server_destroy (&server);
    return 0;
}
