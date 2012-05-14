var zmq = require('zmq')
var subscriber = zmq.socket('sub')

subscriber.on("message", function(reply) {
  console.log('Received message: ', reply.toString());
})

subscriber.connect("tcp://localhost:8688")
subscriber.subscribe("")

process.on('SIGINT', function() {
  subscriber.close()
  console.log('\nClosed')
})
