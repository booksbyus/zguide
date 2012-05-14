var zmq = require('zmq')
var subscriber = zmq.socket('sub')
var client = zmq.socket('req')

subscriber.on('message', function(reply) {
  console.log('Received message: ', reply.toString());
})

subscriber.connect('tcp://localhost:8688')
subscriber.subscribe('')

client.connect('tcp://localhost:8888')
client.send('SYNC')

process.on('SIGINT', function() {
  subscriber.close()
  client.close()
})
