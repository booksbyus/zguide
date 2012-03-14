// Reading from multiple sockets.
// This version listens for emitted 'message' events.

var zmq = require('zmq')

// Connect to task ventilator
var receiver = zmq.socket('pull')

receiver.on('message', function(msg) {
  console.log("From Task Ventilator:", msg.toString())
})

// Connect to weather server.
var subscriber = zmq.socket('sub')

subscriber.subscribe('10001')
subscriber.on('message', function(msg) {
  console.log("Weather Update:", msg.toString())
})

receiver.connect('tcp://localhost:5557')
subscriber.connect('tcp://localhost:5556')
