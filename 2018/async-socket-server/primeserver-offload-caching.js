// Prime server that offloads the actual computations to worker subprocesses
// and uses Redis to cache results.
//
// Requires a Redis server to run in the background, listening on the default
// port.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
var child_process = require('child_process');
var net = require('net');
var redis = require('redis');
var utils = require('./utils.js');

// Create a Redis client. This connects to a Redis server running on the local
// machine at the default port.
var redis_client = redis.createClient();

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

    var cachekey = 'primecache:' + num;
    redis_client.get(cachekey, (err, res) => {
      if (err) {
        console.log('redis client error', err);
      } else {
        if (res === null) {
          var worker = child_process.fork('./primeworker.js');
          worker.send(num);
          worker.on('message', message => {
            var answer = message.result ? 'prime' : 'composite';
            redis_client.set(cachekey, answer, (err, res) => {
              if (err) {
                console.log('redis client error', err);
              } else {
                conn.write(answer + '\n');
                console.log('... %d is %s', num, answer);
              }
            });
          });
        } else {
          // The strings 'prime' or 'composite' are stored in the Redis cache.
          console.log('cached num %d is %s', num, res);
          conn.write(res + '\n');
        }
      }
    });
  }

  function onConnClose() {
    console.log('connection from %s closed', remoteAddress);
  }

  function onConnError(err) {
    console.log('connection %s error: %s', remoteAddress, err.message);
  }
}
