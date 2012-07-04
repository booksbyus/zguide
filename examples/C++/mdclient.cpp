//
//  Majordomo Protocol client example
//  Uses the mdcli API to hide all MDP aspects
//
//  Lets us 'build mdclient' and 'build all'
//
//     Andreas Hoelzlwimmer <andreas.hoelzlwimmer@fh-hagenberg.at>
//
#include "mdcliapi.hpp"

int main (int argc, char *argv [])
{
    int verbose = (argc > 1 && strcmp (argv [1], "-v") == 0);

    mdcli session ("tcp://localhost:5555", verbose);

    int count;
    for (count = 0; count < 100000; count++) {
        zmsg * request = new zmsg("Hello world");
        zmsg * reply = session.send ("echo", request);
        if (reply) {
            delete reply;
        } else {
            break;              //  Interrupt or failure
        }
    }
    std::cout << count << " requests/replies processed" << std::endl;
    return 0;
}
