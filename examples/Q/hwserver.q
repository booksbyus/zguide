//  Hello World server
//  Binds REP socket to tcp://*:5555
//  Expects "Hello" from client, replies with "World"
\l qzmq.q
ctx:zctx.new[]
//  Socket to talk to clients
responder:zsocket.new[ctx; zmq`REP]
port:zsocket.bind[responder; `$"tcp://*:5555"]
while[1b and not zctx.interrupted[];
  //  Wait for next request from client
  s:zmsg.recv responder;
  //  Do some 'work'
  zclock.sleep 1;
  //  Send reply back to client
  m1:zmsg.new[];
  zmsg.push[m1; zframe.new["World"]];
  zmsg.send[m1; responder]]
//  We never get here but if we did, this would how we end
zsocket.destroy[ctx; responder]
zctx.destroy[ctx]

