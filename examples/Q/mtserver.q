//  Multithreaded Hello World server
\l qzmq.q

worker_routine:{[args; ctx; pipe]
    //  Socket to talk to dispatcher
    receiver:zsocket.new[ctx; zmq.REP];
    zsocket.connect[receiver; `inproc://workers];
    while[1b;
        s:zstr.recv[receiver];
        //  Do some 'work'
        zclock.sleep 1;
        //  Send reply back to client
        zstr.send[receiver; "World"]];
    zsocket.destroy[ctx; receiver]}

ctx:zctx.new[]

//  Socket to talk to clients
clients:zsocket.new[ctx; zmq.ROUTER]
clientsport:zsocket.bind[clients; `$"tcp://*:5555"]

//  Socket to talk to workers
workers:zsocket.new[ctx; zmq.DEALER]
workersport:zsocket.bind[workers; `inproc://workers]

//  Launch pool of worker threads
do[5; zthread.fork[ctx; `worker_routine; 0]]
//  Connect work threads to client threads via a queue
rc:libzmq.device[zmq.QUEUE; clients; workers]
if[rc<>-1; '`fail]

//  We never get here but clean up anyhow
zsocket.destroy[ctx; clients]
zsocket.destroy[ctx; workers]
zctx.destroy[ctx]
\\
