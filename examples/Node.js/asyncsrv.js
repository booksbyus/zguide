cluster = require('cluster')
, zmq = require('zmq')
, backAddr  = 'tcp://127.0.0.1:12345'
, frontAddr = 'tcp://127.0.0.1:12346'
, clients = 5
, workers = 2;

// We do this bit repeatedly. Should use with connect or bindSync.
function makeASocket(sockType, idPrefix, addr, bindSyncOrConnect) {
  var sock = zmq.socket(sockType)
  sock.identity = idPrefix + process.pid
  // call the function name in bindSyncOrConnect
  sock[bindSyncOrConnect](addr)
  return sock
}

function clientTask(){
  var sock = makeASocket('dealer', 'client', frontAddr, 'connect')

  var count = 0;
  var interval = setInterval(function() {
    sock.send('request ' + count++)
    if (count >= 10){
      sock.close()
  		cluster.worker.kill() // Done after 10 messages
    }
  }, Math.ceil(Math.random() * 500))

	sock.on('message', function(data) {
    var args = Array.apply(null, arguments)
		console.log(sock.identity + " <- '" + args + "'");
	})
}

function serverTask(){
  var backSvr = makeASocket('dealer', 'back', backAddr, 'bindSync')
  backSvr.on('message', function(){
    var args = Array.apply(null, arguments)
    frontSvr.send(args)
  })

  var frontSvr = makeASocket('router', 'front', frontAddr, 'bindSync')
  frontSvr.on('message', function(){
    var args = Array.apply(null, arguments)
    backSvr.send(args)
  })
}

function workerTask(){
  var sock = makeASocket('dealer', 'wkr', backAddr , 'connect')

	sock.on('message', function() {
    var args = Array.apply(null, arguments)

    var replies = Math.ceil(Math.random() * 4);
    var count = 0;
    var interval = setInterval(function(){
      sock.send([args[0], '', 'response ' + count++])
      if (count == replies){
        clearInterval(interval)
      }
    }, Math.floor(Math.random() * 10)) // sleep a small random time
	})
}

// Node process management noise below
if (cluster.isMaster) {
 // create the workers and clients.
 // Use env variables to dictate client or worker
 for (var i = 0; i < workers; i++) {
    cluster.fork({ "TYPE": 'worker'})
  }
  for (var i = 0; i < clients; i++)  {
    cluster.fork({ "TYPE": 'client' })
  }

  cluster.on('death', function(worker) {
    console.log('worker ' + worker.pid + ' died');
  });

 var deadClients = 0;
 cluster.on('disconnect', function(worker) {
   deadClients++
   if (deadClients === clients) {
     console.log('finished')
     process.exit(0)
   }
 });

  serverTask()
} else {
 if (process.env.TYPE === 'client') {
   clientTask()
 } else {
   workerTask()
 }
}
