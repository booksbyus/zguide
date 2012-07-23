// Weather proxy device in Node.js

var zmq      = require('zmq')
  , frontend = zmq.socket('sub')
  , backend  = zmq.socket('pub');

backend.bindSync("tcp://10.1.1.0:8100");

frontend.subscribe('');
frontend.connect("tcp://192.168.55.210:5556");
frontend.on('message', function() {
  // all parts of a message come as function arguments
  var args = Array.apply(null, arguments);
  backend.send(args);
});
