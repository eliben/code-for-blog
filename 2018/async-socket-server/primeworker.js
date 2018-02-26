// This file should be used as a subprocess (loaded with the child_process
// module).
//
// It receives a number as a message and returns true/false if the numbers
// are/aren't prime. It exits once it returns the answer. It also emulates a
// delay proportional to the number in milliseconds.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
var utils = require('./utils.js');

process.on('message', message => {
  console.log('[child %d] received message from server:', process.pid, message);

  // Compute the result (with emulate ddelay) and send back a message.
  process.send({task: message, result: utils.isPrime(message, true)});
  process.disconnect();
  console.log('[child %d] exiting', process.pid);
  process.exit();
});
