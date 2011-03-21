//
//  Test kvmsg class
//
#include "kvmsg.h"

int main (int argc, char *argv [])
{
    int verbose = (argc > 1 && strcmp (argv [1], "-v") == 0);
    kvmsg_test (verbose);
    return 0;
}

