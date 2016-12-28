//  Broker peering simulation (part 1)
//  Prototypes the state flow

var zmq = require('zeromq')
  , util = require('util');

if (process.argv.length < 3) {
	console.log('usage: node peering1.js me [you ...]');
	process.exit(0);
}

var self = process.argv[2];
console.log("I: preparing broker at %s…", self);

// flag for stopping timer
var done = false;

//
// Backend
//

var statebe = zmq.socket('pub');
statebe.bindSync(util.format("ipc://%s-state.ipc", self));

//
// Frontend
//

var statefe = zmq.socket('sub');
statefe.subscribe('');
for (var i = 3; i < process.argv.length; i++) {
	var peer = process.argv[i];
	console.log("I: connecting to state backend at '%s'", peer);
	statefe.connect(util.format("ipc://%s-state.ipc", peer));
}

process.on('SIGINT', function() {
  done = true;
  statebe.close();
  statefe.close();
});

//  The main loop sends out status messages to peers, and collects
//  status messages back from peers. 

statefe.on('message', function(peer_name, available) {
	console.log("%s - %s workers free", peer_name, available);
});

function sendWorkerAvailability() {
	if (done) {
		return;
	}
	var num_workers = util.format("%d", Math.floor(10 * Math.random()));
	console.log("sending update: %s has %s", self, num_workers);
	statebe.send([ self, num_workers ]);

	var next_send_delay = Math.floor(3000 * Math.random());
	setTimeout(sendWorkerAvailability, next_send_delay);
}

// Start worker update timer loop
sendWorkerAvailability();