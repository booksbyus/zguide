void
connect (String address, String service)
{
    ZMsg msg = new ZMsg();
    msg.add("CONNECT");
    msg.add(address);
    msg.add(service);
    msg.send(this.pipe);
}
