exports.isPrime = function(n) {
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
