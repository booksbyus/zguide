int address;
for (address = 1; address < 255; address++)
    zsocket_connect (listener, "tcp://192.168.55.%d:9000", address);
