// Task worker in node.js
// Connects PULL socket to tcp://localhost:5557
// Collects workloads from ventilator via that socket
// Connects PUSH socket to tcp://localhost:5558
// Sends results to sink via that socket

var zmq      = require('zmq')
  , receiver = zmq.socket('pull')
  , sender   = zmq.socket('push');

receiver.on('message', function(buf) {
  var msec = parseInt(buf.toString(), 10);

  // simple progress indicator for the viewer
  process.stdout.write(buf.toString() + ".");

  // do the work
  // not a great node sample for zeromq,
  // node receives messages while timers run.
  setTimeout(function() {
    sender.send("");
  }, msec);
});

receiver.connect('tcp://localhost:5557');
sender.connect('tcp://localhost:5558');

process.on('SIGINT', function() {
  receiver.close();
  sender.close();
  process.exit();
});
