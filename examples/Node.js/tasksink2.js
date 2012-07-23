// Task sink in Node.js, design 2
// Adds a pub-sub flow to send kill signal to workers

var zmq        = require('zmq')
  , receiver   = zmq.socket('pull')
  , controller = zmq.socket('pub');

var started = false
  , i = 0
  , label = "Total elapsed time";

receiver.on('message', function() {
  // wait for start of batch
  if (!started) {
    console.time(label);
    started = true;

  // process 100 confirmations
  } else {
    i += 1;
    process.stdout.write(i % 10 === 0 ? ':' : '.');
    if (i === 100) {
      console.timeEnd(label);
      controller.send("KILL");
      controller.close();
      receiver.close();
      process.exit();
    }
  }
});

receiver.bindSync("tcp://*:5558");
controller.bindSync("tcp://*:5559");
