// Hello World client in Node.js
// Connects REQ socket to tcp://localhost:5559
// Sends "Hello" to server, expects "World" back

var zmq       = require('zmq')
  , requester = zmq.socket('req');

requester.connect('tcp://localhost:5559');
var replyNbr = 0;
requester.on('message', function(msg) {
  console.log('got reply', replyNbr, msg.toString());
  replyNbr += 1;
});

for (var i = 0; i < 10; ++i) {
  requester.send("Hello");
}

