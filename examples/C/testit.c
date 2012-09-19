#include <czmq.h>
#define CHUNK_SIZE  250000

int main (void)
{
    FILE *file = fopen ("testdata", "r");
    assert (file);

    size_t offset = 0;
    byte *data = malloc (CHUNK_SIZE);
    assert (data);
    while (true) {
        fseek (file, offset, SEEK_SET);
        size_t size = fread (data, 1, CHUNK_SIZE, file);
        if (size == 0)
            break;
        offset += CHUNK_SIZE;
    }
    free (data);
    fclose (file);
    return 0;
}

