//  Last value cache
//  Uses XPUB subscription messages to re-send data

var zmq = require('zeromq');
var frontEnd = zmq.socket('sub');
var backend = zmq.socket('xpub');
var cache = {};

frontEnd.connect('tcp://127.0.0.1:5557');
frontEnd.subscribe('');

backend.bindSync('tcp://*:5558');

frontEnd.on('message', function(topic, message) {
  cache[topic] = message;
  backend.send([topic, message]);
});

backend.on('message', function(frame) {
  //  frame is one byte 0=unsub or 1=sub, followed by topic
  if (frame[0] === 1) {
    var topic = frame.slice(1);
    var previous = cache[topic];

    console.log('Sending cached topic ' + topic);

    if (typeof previous !== 'undefined') {
      backend.send([topic, previous]);
    }
  }
});

process.on('SIGINT', function() {
  frontEnd.close();
  backend.close();
  console.log('\nClosed')
});
