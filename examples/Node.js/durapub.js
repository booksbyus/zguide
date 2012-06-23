// durapub: Publisher for Durable Subscriber
// Marc Harter <wavded@gmail.com>

var zmq = require('zmq')

var sync = zmq.socket('pull')
sync.bind('tcp://127.0.0.1:5564')

var pub = zmq.socket('pub')
pub.bind('tcp://127.0.0.1:5565')

// Subscriber notifies when ready to receive messages
sync.on('message', function (data) {
   sync.close() // close sync after connection established to avoid resending sent data

   var updates = 0
   var interval = setInterval(function () {
      pub.send('Update ' + updates++)
      if (updates >= 10) {
         pub.send('END')
         clearInterval(interval)
      }
   }, 1000)
})

process.on('exit', pub.close)
