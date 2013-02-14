while (true) {
    client.send ("Hello");
    String reply = client.recvStr ();
    if (reply == null)
        break;              //  Interrupted
    System.out.printf ("Client: %s\n", reply);
    Thread.sleep (1000);
}
