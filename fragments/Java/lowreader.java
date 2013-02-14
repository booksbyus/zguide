while (true) {
    //  Get one address frame and empty delimiter
    byte[] address = new byte[255];
    int address_size = worker.recv (address, 0, 255, 0);
    if (address_size == -1)
        break;

    byte[] empty = new byte[1];
    int empty_size = worker.recv (empty, 0, 1, 0);
    assert (empty_size <= 0);
    if (empty_size == -1)
        break;

    //  Get request, send reply
    byte[] request = new byte[256];
    int request_size = worker.recv (request, 0, 255, 0);
    if (request_size == -1)
        return null;
    System.out.printf ("Worker: %s\n", new String (request));
    
    worker.send (address.getBytes (), ZMQ.SNDMORE);
    worker.send (empty.getBytes (), ZMQ.SNDMORE);
    worker.send ("OK".getBytes (), 0);
}
