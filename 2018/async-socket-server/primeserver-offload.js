// Prime server that offloads the actual computations to worker subprocesses.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.

var child_process = require('child_process');
var primeworker = child_process.fork('./primeworker.js');


