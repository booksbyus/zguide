int main (int argc, char *argv [])
{
    //  Initialize context for talking to tasks
    zctx_t *ctx = zctx_new ();
    zctx_set_linger (ctx, 100);

    //  Get number of interfaces to simulate, default 100
    int max_interface = 100;
    int nbr_interfaces = 0;
    if (argc > 1)
        max_interface = atoi (argv [1]);

    //  We address interfaces as an array of pipes
    void **pipes = zmalloc (sizeof (void *) * max_interface);

    //  We will randomly start and stop interface threads
    while (!zctx_interrupted) {
        uint index = randof (max_interface);
        //  Toggle interface thread
        if (pipes [index]) {
            zstr_send (pipes [index], "STOP");
            zsocket_destroy (ctx, pipes [index]);
            pipes [index] = NULL;
            zclock_log ("I: Stopped interface (%d running)",
                --nbr_interfaces);
        }
        else {
            pipes [index] = zthread_fork (ctx, interface_task, NULL);
            zclock_log ("I: Started interface (%d running)",
                ++nbr_interfaces);
        }
        //  Sleep ~750 msecs randomly so we smooth out activity
        zclock_sleep (randof (500) + 500);
    }
    zctx_destroy (&ctx);
    return 0;
}
