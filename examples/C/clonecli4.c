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
    dhash_handler (dhash, s_handler, "", NULL);
    
    //  Set random tuples into the distributed hash
    while (!s_interrupted) {
        //  Set random value, check it was stored
        char key [10];
        char value [10];
        sprintf (key, randof (10000));
        sprintf (value, randof (1000000));
        dhash_set (dhash, key, value);
        char *check = dhash_get (dhash, key);
        assert (streq (value, check));
        
        //  Handle any updates we received
        char *key = dhash_sniff (dhash);
        while (key) {
            printf ("I: updated key=%s value=%s\n", key, dhash_get (dhash, key));
            char *key = dhash_sniff (dhash);
        }
        sleep (1);
    }
    dhash_destroy (&dhash);
    return 0;
}
