//
//  Clone client model 4
//

//  Lets us build this source without creating a library
#include "clone.c"

int main (void)
{
    //  Create distributed hash instance
    clone_t *clone = clone_new ();

    //  Specify primary and backup servers
    clone_connect (clone, "tcp://localhost", "5551");
    clone_connect (clone, "tcp://localhost", "5561");

    //  Set random tuples into the distributed hash
    while (!s_interrupted) {
        //  Set random value, check it was stored
        char key [10];
        char value [10];
        sprintf (key, "%d", randof (10000));
        sprintf (value, "%d", randof (1000000));
        clone_set (clone, key, value);
        sleep (1);
    }
    clone_destroy (&clone);
    return 0;
}
