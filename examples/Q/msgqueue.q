//  Simple message queuing broker
//  Same as request-reply broker but using QUEUE device
\l qzmq.q
ctx:zctx.new[]
//  Socket facing clients
frontend:zsocket.new[ctx; zmq.ROUTER]
frontport:zsocket.bind[frontend; `$"tcp://*:5559"]

//  Socket facing services
backend:zsocket.new[ctx; zmq.DEALER]
backport:zsocket.bind[backend; `$"tcp://*:5560"]

//  Start built-in device
rc:libzmq.device[zmq.QUEUE; frontend; backend]

//  We never get here…
zsocket.destroy[ctx; frontend]
zsocket.destroy[ctx; backend]
zctx.destroy[ctx]
\\

