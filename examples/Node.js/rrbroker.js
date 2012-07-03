// Simple request-reply broker in Node.js

var zmq      = require('zmq')
  , frontend = zmq.socket('router')
  , backend  = zmq.socket('dealer');

frontend.bindSync('tcp://*:5559');
backend.bindSync('tcp://*:5560');

frontend.on('message', function() {
  // Note that separate message parts come as function arguments.
  var args = Array.apply(null, arguments);
  // Pass array of strings/buffers to send multipart messages.
  backend.send(args);
});

backend.on('message', function() {
  var args = Array.apply(null, arguments);
  frontend.send(args);
});
