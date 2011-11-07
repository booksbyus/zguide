var context = require('zeromq')
var publisher = context.createSocket('pub')
var server = context.createSocket('rep')
var pending = 0

server.on('message', function(request) {
    pending++
    console.log(request.toString(), pending)
    server.send('OK')
    if (pending > 0)
        publisher.send(pending + ' subscribers connected.')
})

server.bind('tcp://*:8888', function(err) {
  if(err)
    console.log(err)
  else
    console.log('Listening on 8888...')
})

publisher.bind('tcp://*:8688', function(err) {
  if(err)
    console.log(err)
  else
    console.log('Listening on 8688...')
})

process.on('SIGINT', function() {
  publisher.close()
  server.close()
})
