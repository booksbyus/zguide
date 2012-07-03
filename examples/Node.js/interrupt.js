// Show how to handle Ctrl+C in Node.js

var zmq = require('zmq')
  , socket = zmq.createSocket('rep');

socket.on('message', function(buf) {
  // echo request back
  socket.send(buf);
});

process.on('SIGINT', function() {
  socket.close();
  process.exit();
});

socket.bindSync('tcp://*:5555');
