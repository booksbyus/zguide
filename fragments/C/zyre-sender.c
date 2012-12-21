#include <zre.h>

int main (int argc, char *argv [])
{
    if (argc < 2) {
        puts ("Syntax: sender filename virtualname");
        return 0;
    }
    printf ("Publishing %s as %s\n", argv [1], argv [2]);
    zre_interface_t *interface = zre_interface_new ();
    zre_interface_publish (interface, argv [1], argv [2]);
    while (true) {
        zmsg_t *incoming = zre_interface_recv (interface);
        if (!incoming)
            break;
        zmsg_dump (incoming);
        zmsg_destroy (&incoming);
    }
    zre_interface_destroy (&interface);
    return 0;
}
