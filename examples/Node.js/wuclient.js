// weather update client in node.js
// connects SUB socket to tcp://localhost:5556
// collects weather updates and finds avg temp in zipcode

var zmq = require('zmq');

console.log("Collecting updates from weather server...");

// Socket to talk to server
var subscriber = zmq.socket('sub');

// Subscribe to zipcode, default is NYC, 10001
var filter = null;
if (process.argv.length > 2) {
  filter = process.argv[2];
} else {
  filter = "10001";
}
console.log(filter);
subscriber.subscribe(filter);

// process 100 updates
var total_temp = 0
  , temps      = 0;
subscriber.on('message', function(data) {
  var pieces      = data.toString().split(" ")
    , zipcode     = parseInt(pieces[0], 10)
    , temperature = parseInt(pieces[1], 10)
    , relhumidity = parseInt(pieces[2], 10);

  temps += 1;
  total_temp += temperature;

  if (temps === 100) {
    console.log([
      "Average temperature for zipcode '",
      filter,
      "' was ",
      (total_temp / temps).toFixed(2),
      " F"].join(""));
    total_temp = 0;
    temps = 0;
  }
});

subscriber.connect("tcp://localhost:5556");
