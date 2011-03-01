//
//  Majordomo client
//
#include "mdcliapi.c"

int main (void)
{
    mdcli_t *mdcli;
    int rc;

    printf (" * mdcli: ");
    mdcli = mdcli_new ("tcp://127.0.0.1:5055");

    mdcli_destroy (&mdcli);
    return 0;

    return 0;
}
