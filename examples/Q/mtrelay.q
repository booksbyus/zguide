//  Multithreaded relay
\l qzmq.q
step1:{[args; ctx; pipe]
    //  Connect to step2 and tell it we're ready
    xmitter:zsocket.new[ctx; zmq.PAIR];
    zsocket.connect[xmitter; `inproc://step2];
    zclock.log "Step 1 ready, signaling step 2";
    zstr.send[xmitter; "READY"];
    zsocket.destroy[ctx; xmitter]}
step2:{[args; ctx; pipe]
    //  Bind inproc socket before starting step1
    receiver:zsocket.new[ctx; zmq.PAIR];
    port:zsocket.bind[receiver; `inproc://step2];
    pipe:zthread.fork[ctx; `step1; 0N];

    //  Wait for signal and pass it on
    zclock.log s:zstr.recv[receiver];
    //  Connect to step3 and tell it we're ready
    xmitter:zsocket.new[ctx; zmq.PAIR];
    zsocket.connect[xmitter; `inproc://step3];
    zclock.log "Step 2 ready, signaling step 3";
    zstr.send[xmitter; "READY"];
    zsocket.destroy[ctx; xmitter]}

ctx:zctx.new[]
//  Bind inproc socket before starting step2
receiver:zsocket.new[ctx; zmq.PAIR]
port:zsocket.bind[receiver; `inproc://step3]
pipe:zthread.fork[ctx; `step2; 0N]

//  Wait for signal
zclock.log s:zstr.recv[receiver]
zclock.log "Test successful!"
zctx.destroy[ctx]
\\
