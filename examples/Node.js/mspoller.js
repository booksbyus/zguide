// Reading from multiple sockets.
// This version listens for emitted 'message' events.

var context = require('zeromq')

// Connect to task ventilator
var receiver = context.createSocket('pull')
receiver.on('message', function(msg) {
  console.log("From Task Ventilator:", msg.toString())
})

// Connect to weather server.
var subscriber = context.createSocket('sub')
subscriber.subscribe('10001')
subscriber.on('message', function(msg) {
  console.log("Weather Update:", msg.toString())
})

receiver.connect('tcp://localhost:5557')
subscriber.connect('tcp://localhost:5556')
