while (1) {
    zmsg_t *zmsg = zmsg_recv (worker);
    printf ("Worker: %s\n", zmsg_body (zmsg));
    zmsg_body_set (zmsg, "OK");
    zmsg_send (&zmsg, worker);
}
