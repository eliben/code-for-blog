Using with curl:

  curl "localhost:8000/get?name=j"
  curl "localhost:8000/set?name=t&val=9"
  curl "localhost:8000/inc?name=t"

Concurrent hammering with ab:

  ab -n 20000 -c 500 "127.0.0.1:8000/inc?name=i"
