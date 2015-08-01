#include "czmq.h"

int main (void)
{
    FILE *handle = fopen ("testdata", "w");
    byte data [1024 * 1024];   
    int count;
    for (count = 0; count < 1024; count++)
        fwrite (data, 1, 1024 * 1024, handle);
    fclose (handle);
    return 0;
}

