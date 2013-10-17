/*  =========================================================================
    ch6_selftest - run self tests for Chapter 6 code examples
    =========================================================================
*/

#include "czmq.h"
#include "mdp_worker.h"
#include "mdp_client.h"
#include "nom_server.h"

int main (int argc, char *argv [])
{
    bool verbose;
    if (argc == 2 && streq (argv [1], "-v"))
        verbose = true;
    else
        verbose = false;

    printf ("Running self tests...\n");
    mdp_worker_test (verbose);
    mdp_client_test (verbose);
    nom_server_test (verbose);
    printf ("Tests passed OK\n");
    return 0;
}
