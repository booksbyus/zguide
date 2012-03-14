// weather update client in node.js
// connects SUB socket to tcp://localhost:5556
// collects weather updates and finds avg temp in zipcode

var zmq = require('zmq')

console.log("Collecting updates from weather server...")

// Socket to talk to server
var subscriber = zmq.socket('sub')

// Subscribe to zipcode, default is NYC, 10001
var filter = null
if(process.argv.length > 2) {
  filter = process.argv[2]
} else {
  filter = "10001"
}
subscriber.subscribe(filter)

// process 100 updates
var total_temp = 0
var temps      = 0
subscriber.on('message', function(data) {
  var pieces = data.toString().split(" ")
  var zipcode = parseInt(pieces[0])
  var temperature = parseInt(pieces[1])
  var relhumidity = parseInt(pieces[2])
  temps += 1
  total_temp += temperature

  if(temps == 100) {
    console.log("Average temperature for zipcode '", filter, "' was", total_temp/100, "F")
    total_temp = 0
    temps = 0
  }
})

subscriber.connect("tcp://localhost:5556")

