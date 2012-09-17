/*  =========================================================================
    mdp_selftest - run self tests

    Runs all selftests.

    =========================================================================
*/

#include "czmq.h"
#include "mdp_worker.h"
#include "mdp_client.h"

int main (int argc, char *argv [])
{
    Bool verbose;
    if (argc == 2 && streq (argv [1], "-v"))
        verbose = TRUE;
    else
        verbose = FALSE;

    printf ("Running self tests...\n");
    mdp_worker_test (verbose);
    mdp_client_test (verbose);
    printf ("Tests passed OK\n");
    return 0;
}
