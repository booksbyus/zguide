// durasub: Durable Subscriber
// Marc Harter <wavded@gmail.com>

var zmq = require('zmq')

var sub = zmq.socket('sub')
sub.identity = 'Hello' // set identity to enable durability
sub.connect('tcp://127.0.0.1:5565')
sub.subscribe('')

var sync = zmq.socket('push')
sync.connect('tcp://127.0.0.1:5564')
sync.send('') // notify publisher to start sending data

sub.on('message', function (data) {
   var message = data.toString()
   console.log(message)
   if (message === 'END') { // on 'END' close connections
      sync.close()
      sub.close()
   }
})
