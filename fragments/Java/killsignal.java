Socket control = context.socket (ZMQ.PUB);
control.bind ("tcp://*:5559");
...
//  Send kill signal to workers
controller.send ("KILL");
