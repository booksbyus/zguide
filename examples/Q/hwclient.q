//  Hello World client
//  Connects REQ socket to tcp://localhost:5555
//  Sends "Hello" to server, expects "World" back
\l qzmq.q
zclock.log "Connecting to hello world server..."
ctx:zctx.new[]
//  Socket to talk to server
requester:zsocket.new[ctx; zmq`REQ]
zsocket.connect[requester; `tcp://127.0.0.1:5555]
do[10; m:zmsg.new[]; zmsg.push[m; f:zframe.new["Hello"]];
  zmsg.send[m; requester]; zmsg.dump[zmsg.recv[requester]]]
zsocket.destroy[ctx; requester]
zctx.destroy[ctx]
\\

