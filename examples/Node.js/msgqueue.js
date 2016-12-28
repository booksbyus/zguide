//  Simple message queuing broker
//  Same as request-reply broker but using shared queue proxy

var zmq = require('zeromq');

//  Socket facing clients
var frontend = zmq.socket('router');
console.log('binding frontend...');
frontend.bindSync('tcp://*:5559');

//  Socket facing services
var backend = zmq.socket('dealer');
console.log('binding backend...');
backend.bindSync('tcp://*:5560');

//  Start the proxy
console.log('starting proxy...');
zmq.proxy(frontend, backend, null);

process.on('SIGINT', function() {
  frontend.close();
  backend.close();
});
