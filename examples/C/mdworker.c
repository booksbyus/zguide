//
//  Majordomo worker
//
#include "mdwrkapi.c"

int main (void)
{
    zmsg_t *reply = NULL;

    mdwrk_t *worker = mdwrk_new ("tcp://localhost:5555", "echo");
    while (1) {
        zmsg_t *request = mdwrk_recv (worker, reply);
        if (request == NULL)
            break;
        zmsg_dump (request);

        //  Echo is complex...
        reply = request;
    }
    mdwrk_destroy (&worker);
    return 0;
}
