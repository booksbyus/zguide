// Weather update server in node.js
// Binds PUB socket to tcp://*:5556
// Publishes random weather updates

var zmq = require('zmq')
  , publisher = zmq.socket('pub');

publisher.bindSync("tcp://*:5556");
publisher.bindSync("ipc://weather.ipc");

function zeropad(num) {
  while (num.length < 5) {
    num = "0" + num;
  }
  return num;
};

function rand(upper, extra) {
  var num = Math.abs(Math.round(Math.random() * upper));
  return num + (extra || 0);
};

while (true) {
  // Get values that will fool the boss
  var zipcode     = rand(100000)
    , temperature = rand(215, -80)
    , relhumidity = rand(50, 10)
    , update      = zeropad(zipcode.toString()) + ' ' + temperature + ' ' + relhumidity;
  publisher.send(update);
}
