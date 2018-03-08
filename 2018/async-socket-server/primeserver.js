// Simple prime server that invokes a CPU-consuming operation for every request.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
var net = require('net');
var utils = require('./utils.js');

var portnum = 8070;
if (process.argv.length > 2) {
  portnum = process.argv[2];
}

var server = net.createServer();
server.on('connection', handleConnection);

server.listen(portnum, function() {
  console.log('Serving on port %d', portnum);
});

function handleConnection(conn) {
  var remoteAddress = conn.remoteAddress + ':' + conn.remotePort;
  console.log('peer %s connected', remoteAddress);

  conn.on('data', onConnData);
  conn.once('close', onConnClose);
  conn.on('error', onConnError);

  function onConnData(d) {
    var num = utils.buf2num(d);
    console.log('num %d', num);

    var answer = utils.isPrime(num, true) ? "prime" : "composite";
    conn.write(answer + '\n');
    console.log('... %d is %s', num, answer);
  }

  function onConnClose() {
    console.log('connection from %s closed', remoteAddress);
  }

  function onConnError(err) {
    console.log('connection %s error: %s', remoteAddress, err.message);
  }
}
