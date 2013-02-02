Context context = ZMQ.context (1);
assert (context != null);
Socket socket = context.socket (ZMQ.REP);
assert (socket != null);
try {
    socket.bind ("tcp://*:5555");
} catch (ZMQException e) {
    System.out.printf ("E: bind failed: %s\n", e.toString ());
    throw e;
}
