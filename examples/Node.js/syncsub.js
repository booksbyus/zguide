var context = require('zeromq')
var subscriber = context.createSocket('sub')
var client = context.createSocket('req')

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
