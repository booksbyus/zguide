//  File Transfer model #2
//  
//  In which the client requests each chunk individually, thus
//  eliminating server queue overflows, but at a cost in speed.

#include <czmq.h>
#define CHUNK_SIZE  250000

static void
client_thread (void *args, zctx_t *ctx, void *pipe)
{
    void *dealer = zsocket_new (ctx, ZMQ_DEALER);
    zsocket_set_hwm (dealer, 1);
    zsocket_connect (dealer, "tcp://127.0.0.1:6000");

    size_t total = 0;       //  Total bytes received
    size_t chunks = 0;      //  Total chunks received

    while (true) {
        //  Ask for next chunk
        zstr_sendfm (dealer, "fetch");
        zstr_sendfm (dealer, "%ld", total);
        zstr_sendf  (dealer, "%ld", CHUNK_SIZE);
        
        zframe_t *chunk = zframe_recv (dealer);
        if (!chunk)
            break;              //  Shutting down, quit
        chunks++;
        size_t size = zframe_size (chunk);
        zframe_destroy (&chunk);
        total += size;
        if (size < CHUNK_SIZE)
            break;              //  Last chunk received; exit
    }
    printf ("%zd chunks received, %zd bytes\n", chunks, total);
    zstr_send (pipe, "OK");
}

static void
free_chunk (void *data, void *arg)
{
    free (data);
}

//  .split File server thread
//  The server thread waits for a chunk request from a client,
//  reads that chunk and sends it back to the client:

static void
server_thread (void *args, zctx_t *ctx, void *pipe)
{
    FILE *file = fopen ("testdata", "r");
    assert (file);

    void *router = zsocket_new (ctx, ZMQ_ROUTER);
    zsocket_set_hwm (router, 1);
    zsocket_bind (router, "tcp://*:6000");
    while (true) {
        //  First frame in each message is the sender identity
        zframe_t *identity = zframe_recv (router);
        if (!identity)
            break;              //  Shutting down, quit
            
        //  Second frame is "fetch" command
        char *command = zstr_recv (router);
        assert (streq (command, "fetch"));
        free (command);

        //  Third frame is chunk offset in file
        char *offset_str = zstr_recv (router);
        size_t offset = atoi (offset_str);
        free (offset_str);

        //  Fourth frame is maximum chunk size
        char *chunksz_str = zstr_recv (router);
        size_t chunksz = atoi (chunksz_str);
        free (chunksz_str);

        //  Read chunk of data from file
        fseek (file, offset, SEEK_SET);
        byte *data = malloc (chunksz);
        assert (data);

        //  Send resulting chunk to client
        size_t size = fread (data, 1, chunksz, file);
        zframe_t *chunk = zframe_new_zero_copy (data, size, free_chunk, NULL);
        zframe_send (&identity, router, ZFRAME_MORE);
        zframe_send (&chunk, router, 0);
    }
    fclose (file);
}

//  The main task is just the same as in the first model.
//  .skip

int main (void)
{
    //  Start child threads
    zctx_t *ctx = zctx_new ();
    zthread_fork (ctx, server_thread, NULL);
    void *client =
    zthread_fork (ctx, client_thread, NULL);
    //  Loop until client tells us it's done
    char *string = zstr_recv (client);
    free (string);
    //  Kill server thread
    zctx_destroy (&ctx);
    return 0;
}
//  .until