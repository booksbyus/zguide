//
//  Clone client model 4
//
//  Lets us 'build clonecli4' and 'build all'
#include "dhash.c"

void
int main (void)
{
    //  Create distributed hash instance
    dhast_t *dhash = dhash_new ();

    //  Specify primary and backup servers
    dhash_server (dhash, "tcp://localhost:5551");
    dhash_server (dhash, "tcp://localhost:5561");

    //  Set random tuples into the distributed hash
    while (!s_interrupted) {
        //  Set random value, check it was stored
        char key [10];
        char value [10];
        sprintf (key, randof (10000));
        sprintf (value, randof (1000000));
        dhash_set (dhash, key, value);
        sleep (1);
    }
    dhash_destroy (&dhash);
    return 0;
}
