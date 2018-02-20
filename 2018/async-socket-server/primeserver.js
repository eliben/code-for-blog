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
    console.log('got num %d', num);

    if (utils.isPrime(num)) {
      conn.write('prime\n');
    } else {
      conn.write('composite\n');
    }
  }

  function onConnClose() {
    console.log('connection from %s closed', remoteAddress);
  }

  function onConnError(err) {
    console.log('connection %s error: %s', remoteAddress, err.message);
  }
}
