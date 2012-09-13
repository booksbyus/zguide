//  File Transfer model #1
//  
//  In which the server sends the entire file to the client in
//  large chunks with no attempt at flow control.

#include <czmq.h>
#define CHUNK_SIZE  250000

static void
client_thread (void *args, zctx_t *ctx, void *pipe)
{
    void *dealer = zsocket_new (ctx, ZMQ_DEALER);
    zsocket_connect (dealer, "tcp://127.0.0.1:6000");
    
    zstr_send (dealer, "fetch");
    size_t total = 0;       //  Total bytes received
    size_t chunks = 0;      //  Total chunks received
    
    while (true) {
        zframe_t *frame = zframe_recv (dealer);
        if (!frame)
            break;              //  Shutting down, quit
        chunks++;
        size_t size = zframe_size (frame);
        zframe_destroy (&frame);
        total += size;
        if (size == 0)
            break;              //  Whole file received
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
//  The server thread reads the file from disk in chunks, and sends
//  each chunk to the client as a separate message. We only have one
//  test file, so open that once and then serve it out as needed:

static void
server_thread (void *args, zctx_t *ctx, void *pipe)
{
    FILE *file = fopen ("testdata", "r");
    assert (file);

    void *router = zsocket_new (ctx, ZMQ_ROUTER);
    //  Default HWM is 1000, which will drop messages here
    //  since we send more than 1,000 chunks of test data,
    //  so set an infinite HWM as a simple, stupid solution:
    zsocket_set_hwm (router, 0);
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

        while (true) {
            byte *data = malloc (CHUNK_SIZE);
            assert (data);
            size_t size = fread (data, 1, CHUNK_SIZE, file);
            zframe_t *chunk = zframe_new_zero_copy (data, size, free_chunk, NULL);
            zframe_send (&identity, router, ZFRAME_REUSE + ZFRAME_MORE);
            zframe_send (&chunk, router, 0);
            if (size == 0)
                break;          //  Always end with a zero-size frame
        }
        zframe_destroy (&identity);
    }
    fclose (file);
}

//  .split File main thread
//  The main task starts the client and server threads; it's easier
//  to test this as a single process with threads, than as multiple
//  processes:

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
