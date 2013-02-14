while (true) {
    ZMsg msg = ZMsg.recvMsg (worker);
    msg.getLast ().reset ("OK");
    msg.send (worker);
}
