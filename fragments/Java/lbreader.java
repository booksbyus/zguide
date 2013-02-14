while (true) {
    //  Get one address frame and empty delimiter
    String address = worker.recvStr ();
    String empty = worker.recvStr ();
    assert (empty.length() == 0);

    //  Get request, send reply
    String request = worker.recvStr ();
    System.out.printf ("Worker: %s\n", request);

    worker.sendMore (address);
    worker.sendMore ("");
    worker.send ("OK");
}
