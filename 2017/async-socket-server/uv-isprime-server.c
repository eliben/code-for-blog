// Primality testing server. Accepts a number and sends back "prime" or
// "composite" after testing the number for primality (in the naive, slow way).
//
// Can be configured via an environment variable to do this in a blocking way,
// without using libuv's work queue (MODE=BLOCK).
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "uv.h"

#include "utils.h"

#define N_BACKLOG 64

#define SENDBUF_SIZE 1024

// State maintained for each connected client.
typedef struct {
  uint64_t number;
  uv_tcp_t* client;
  char sendbuf[SENDBUF_SIZE];
  int sendbuf_end;
} peer_state_t;

// Sets sendbuf/sendbuf_end in the given state to the contents of the
// NULL-terminated string passed as 'str'.
void set_peer_sendbuf(peer_state_t* state, const char* str) {
  int i = 0;
  for (; str[i]; ++i) {
    assert(i < SENDBUF_SIZE);
    state->sendbuf[i] = str[i];
  }
  state->sendbuf_end = i;
}

void on_alloc_buffer(uv_handle_t* handle, size_t suggested_size,
                     uv_buf_t* buf) {
  buf->base = (char*)xmalloc(suggested_size);
  buf->len = suggested_size;
}

void on_client_closed(uv_handle_t* handle) {
  uv_tcp_t* client = (uv_tcp_t*)handle;
  // The client handle owns the peer state storing its address in the data
  // field, so we free it here.
  if (client->data) {
    free(client->data);
  }
  free(client);
}

// Naive primality test, iterating all the way to sqrt(n) to find numbers that
// divide n.
bool isprime(uint64_t n) {
  if (n % 2 == 0) {
    return n == 2 ? true : false;
  }

  for (uint64_t r = 3; r * r <= n; r += 2) {
    if (n % r == 0) {
      return false;
    }
  }
  return true;
}

void on_sent_response(uv_write_t* req, int status) {
  if (status) {
    die("Write error: %s\n", uv_strerror(status));
  }
  free(req);
}

// Runs in a separate thread, can do blocking/time-consuming operations.
void on_work_submitted(uv_work_t* req) {
  peer_state_t* peerstate = (peer_state_t*)req->data;
  printf("work submitted: %" PRIu64 "\n", peerstate->number);
  if (isprime(peerstate->number)) {
    set_peer_sendbuf(peerstate, "prime\n");
  } else {
    set_peer_sendbuf(peerstate, "composite\n");
  }
}

void on_work_completed(uv_work_t* req, int status) {
  if (status) {
    die("on_work_completed error: %s\n", uv_strerror(status));
  }
  peer_state_t* peerstate = (peer_state_t*)req->data;
  printf("work completed: %" PRIu64 "\n", peerstate->number);
  uv_buf_t writebuf = uv_buf_init(peerstate->sendbuf, peerstate->sendbuf_end);
  uv_write_t* writereq = (uv_write_t*)xmalloc(sizeof(*writereq));
  writereq->data = peerstate;
  int rc;
  if ((rc = uv_write(writereq, (uv_stream_t*)peerstate->client, &writebuf, 1,
                     on_sent_response)) < 0) {
    die("uv_write failed: %s", uv_strerror(rc));
  }
  free(req);
}

void on_peer_read(uv_stream_t* client, ssize_t nread, const uv_buf_t* buf) {
  if (nread < 0) {
    if (nread != UV_EOF) {
      fprintf(stderr, "Read error: %s\n", uv_strerror(nread));
    }
    uv_close((uv_handle_t*)client, on_client_closed);
  } else if (nread == 0) {
    // From the documentation of uv_read_cb: nread might be 0, which does not
    // indicate an error or EOF. This is equivalent to EAGAIN or EWOULDBLOCK
    // under read(2).
  } else {
    // nread > 0
    assert(buf->len >= nread);
    int rc;

    // Parse the number from client request: assume for simplicity the request
    // all arrives at the same time and contains only digits (possibly followed
    // by non-digits like a newline).
    uint64_t number = 0;
    for (int i = 0; i < nread; ++i) {
      char c = buf->base[i];
      if (isdigit(c)) {
        number = number * 10 + (c - '0');
      } else {
        break;
      }
    }
    peer_state_t* peerstate = (peer_state_t*)client->data;
    peerstate->client = (uv_tcp_t*)client;
    peerstate->number = number;

    char* mode = getenv("MODE");
    if (mode && !strcmp(mode, "BLOCK")) {
      // BLOCK mode: compute isprime synchronously, blocking the callback.
      printf("Got %zu bytes\n", nread);
      printf("Num %" PRIu64 "\n", number);

      uint64_t t1 = uv_hrtime();
      if (isprime(number)) {
        set_peer_sendbuf(peerstate, "prime\n");
      } else {
        set_peer_sendbuf(peerstate, "composite\n");
      }
      uint64_t t2 = uv_hrtime();
      printf("Elapsed %" PRIu64 " ns\n", t2 - t1);

      uv_buf_t writebuf =
          uv_buf_init(peerstate->sendbuf, peerstate->sendbuf_end);
      uv_write_t* writereq = (uv_write_t*)xmalloc(sizeof(*writereq));
      writereq->data = peerstate;
      if ((rc = uv_write(writereq, (uv_stream_t*)client, &writebuf, 1,
                         on_sent_response)) < 0) {
        die("uv_write failed: %s", uv_strerror(rc));
      }
    } else {
      // Otherwise, compute isprime on the work queue, without blocking the
      // callback.
      uv_work_t* work_req = (uv_work_t*)xmalloc(sizeof(*work_req));
      work_req->data = peerstate;
      if ((rc = uv_queue_work(uv_default_loop(), work_req, on_work_submitted,
                              on_work_completed)) < 0) {
        die("uv_queue_work failed: %s", uv_strerror(rc));
      }
    }
  }
  free(buf->base);
}

void on_peer_connected(uv_stream_t* server, int status) {
  if (status < 0) {
    fprintf(stderr, "Peer connection error: %s\n", uv_strerror(status));
    return;
  }

  // client will represent this peer; it's allocated on the heap and only
  // released when the client disconnects. The client holds a pointer to
  // peer_state_t in its data field; this peer state tracks the protocol state
  // with this client throughout interaction.
  uv_tcp_t* client = (uv_tcp_t*)xmalloc(sizeof(*client));
  int rc;
  if ((rc = uv_tcp_init(uv_default_loop(), client)) < 0) {
    die("uv_tcp_init failed: %s", uv_strerror(rc));
  }
  client->data = NULL;

  if (uv_accept(server, (uv_stream_t*)client) == 0) {
    struct sockaddr_storage peername;
    int namelen = sizeof(peername);
    if ((rc = uv_tcp_getpeername(client, (struct sockaddr*)&peername,
                                 &namelen)) < 0) {
      die("uv_tcp_getpeername failed: %s", uv_strerror(rc));
    }
    report_peer_connected((const struct sockaddr_in*)&peername, namelen);

    // Initialize the peer state for a new client.
    peer_state_t* peerstate = (peer_state_t*)xmalloc(sizeof(*peerstate));
    peerstate->sendbuf_end = 0;
    client->data = peerstate;

    // Start reading on the peer socket.
    if ((rc = uv_read_start((uv_stream_t*)client, on_alloc_buffer,
                            on_peer_read)) < 0) {
      die("uv_read_start failed: %s", uv_strerror(rc));
    }
  } else {
    uv_close((uv_handle_t*)client, on_client_closed);
  }
}

int main(int argc, const char** argv) {
  setvbuf(stdout, NULL, _IONBF, 0);

  int portnum = 8070;
  if (argc >= 2) {
    portnum = atoi(argv[1]);
  }
  printf("Serving on port %d\n", portnum);

  int rc;
  uv_tcp_t server;
  if ((rc = uv_tcp_init(uv_default_loop(), &server)) < 0) {
    die("uv_tcp_init failed: %s", uv_strerror(rc));
  }

  struct sockaddr_in addr;
  if ((rc = uv_ip4_addr("0.0.0.0", portnum, &addr)) < 0) {
    die("uv_ip4_addr failed: %s", uv_strerror(rc));
  }

  if ((rc = uv_tcp_bind(&server, (const struct sockaddr*)&addr, 0)) < 0) {
    die("uv_tcp_bind failed: %s", uv_strerror(rc));
  }

  // Listen on the socket for new peers to connect. When a new peer connects,
  // the on_peer_connected callback will be invoked.
  if ((rc = uv_listen((uv_stream_t*)&server, N_BACKLOG, on_peer_connected)) <
      0) {
    die("uv_listen failed: %s", uv_strerror(rc));
  }

  // Run the libuv event loop.
  uv_run(uv_default_loop(), UV_RUN_DEFAULT);

  // If uv_run returned, close the default loop before exiting.
  return uv_loop_close(uv_default_loop());
}
