// Task ventilator in node.js
// Binds PUSH socket to tcp://localhost:5557
// Sends batch of tasks to workers via that socket.

var zmq = require('zmq');
process.stdin.resume();
require('tty').setRawMode(true);

// Socket to send messages on
var sender = zmq.socket('push');
sender.bindSync("tcp://*:5557");

var i          = 0
  , total_msec = 0;

function work() {
  console.log("Sending tasks to workers...");

  // The first message is "0" and signals start of batch
  sender.send("0");

  // send 100 tasks
  while (i < 100) {
    var workload = Math.abs(Math.round(Math.random() * 100)) + 1;
    total_msec += workload;
    process.stdout.write(workload.toString() + ".");
    sender.send(workload.toString());
    i++;
  }
  console.log("Total expected cost:", total_msec, "msec");
  sender.close();
  process.exit();
};

console.log("Press enter when the workers are ready...");
process.stdin.on("data", function() {
  if (i === 0) {
    work();
  }
  process.stdin.pause();
});
