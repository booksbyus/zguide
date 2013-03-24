mdcliapi2 client = new mdcliapi2 (String broker);
void      client.destroy();
boolean   client.send(String service, ZMsg request);
ZMsg      client.recv();
