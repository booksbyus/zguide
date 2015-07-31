//  Demonstrate request-reply identities

var zmq = require("zmq"),
    zhelpers = require('./zhelpers');

var sink = zmq.socket("router");
sink.bind("inproc://example");

sink.on("message", zhelpers.dumpFrames);

//  First allow 0MQ to set the identity
var anonymous = zmq.socket("req");
anonymous.connect("inproc://example");
anonymous.send("ROUTER uses generated 5 byte identity");

//  Then set the identity ourselves
var identified = zmq.socket("req");
identified.identity = "PEER2";
identified.connect("inproc://example");
identified.send("ROUTER uses REQ's socket identity");

setTimeout(function() {
  anonymous.close();
  identified.close();
  sink.close();
}, 250);
