basic-server.go doesn't do locking on the map, so when exercised with
concurrent connections we'll get a "concurrent map writes" error.

Using with curl:

  curl "localhost:8000/get?name=j"
  curl "localhost:8000/set?name=t&val=9"
  curl "localhost:8000/inc?name=t"

Concurrent hammering with ab:

  ab -n 20000 -c 500 "127.0.0.1:8000/inc?name=i"

Note: write something about https://github.com/golang/go/issues/20060 -- pointer
receivers needed - folks on the issue say "go vet" should catch this - try it
out.

Also interesting:
https://github.com/golang/go/wiki/MutexOrChannel
