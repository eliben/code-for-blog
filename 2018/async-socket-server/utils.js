// Common utils.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.

// Check if n is prime, returning a boolean. The delay parameter is optional -
// if it's true the function will block for n milliseconds before computing the
// answer.
exports.isPrime = function(n, delay) {
  if (delay === true) {
    sleep(n);
  }

  if (n % 2 == 0) {
    return n == 2 ? true : false;
  }

  for (var r = 3; r * r <= n; r += 2) {
    if (n % r == 0) {
      return false;
    }
  }
  return true;
}

// Parse the given a buffer into a number. buf is of class Buffer; it stores the
// ascii representation of the number followed by some non-digits (like a
// newline).
exports.buf2num = function(buf) {
  var num = 0;
  var code0 = '0'.charCodeAt(0);
  var code9 = '9'.charCodeAt(0);
  for (var i = 0; i < buf.length; ++i) {
    if (buf[i] >= code0 && buf[i] <= code9) {
      num = num * 10 + buf[i] - code0;
    } else {
      break;
    }
  }
  return num;
}

// Blocking sleep for the given number of milliseconds. Uses a spin-loop to
// block; note that this loads the CPU and is only useful for simulating load.
function sleep(ms) {
  var awake_time = new Date().getTime() + ms;
  while (awake_time > new Date().getTime()) {
  }
}
