//
//  Test kvmsg class
//
#include "kvmsg.c"

int main (int argc, char *argv [])
{
    int verbose = (argc > 1 && streq (argv [1], "-v"));
    kvmsg_test (verbose);
    return 0;
}

