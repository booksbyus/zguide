//  Demonstrate identities as used by the request-reply pattern.
\l qzmq.q
ctx:zctx.new[]
sink:zsocket.new[ctx; zmq`ROUTER]
port:zsocket.bind[sink; `inproc://example]
//  First allow 0MQ to set the identity
anonymous:zsocket.new[ctx; zmq`REQ]
zsocket.connect[anonymous; `inproc://example]
m0:zmsg.new[]
zmsg.push[m0; zframe.new["ROUTER uses a generated UUID"]]
zmsg.send[m0; anonymous]
zmsg.dump[zmsg.recv[sink]]
//  Then set the identity ourself
identified:zsocket.new[ctx; zmq`REQ]
zsockopt.set_identity[identified; "Hello"]
zsocket.connect[identified; `inproc://example]
m1:zmsg.new[]
zmsg.push[m1; zframe.new["ROUTER socket users REQ's socket identity"]]
zmsg.send[m1; identified]
zmsg.dump[zmsg.recv[sink]]
zsocket.destroy[ctx; sink]
zsocket.destroy[ctx; anonymous]
zsocket.destroy[ctx; identified]
zctx.destroy[ctx]
\\

