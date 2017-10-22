#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "uv.h"

#include "utils.h"

#define N_BACKLOG 64

// Wraps malloc with error checking: dies if malloc fails.
void* xmalloc(size_t size) {
  void* ptr = malloc(size);
  if (!ptr) {
    die("malloc failed");
  }
  return ptr;
}

typedef enum { INITIAL_ACK, WAIT_FOR_MSG, IN_MSG } ProcessingState;

#define SENDBUF_SIZE 1024

typedef struct {
  ProcessingState state;
  char sendbuf[SENDBUF_SIZE];
  int sendbuf_end;
} peer_state_t;

void on_alloc_buffer(uv_handle_t* handle, size_t suggested_size,
                     uv_buf_t* buf) {
  buf->base = (char*)xmalloc(suggested_size);
  buf->len = suggested_size;
}

void on_handle_closed(uv_handle_t* handle) {
  free(handle);
}

void on_wrote_init_ack(uv_write_t* req, int status) {
  if (status) {
    die("Write error: %s\n", uv_strerror(status));
  }
  peer_state_t* peerstate = (peer_state_t*)req->data;
  peerstate->state = WAIT_FOR_MSG;
  peerstate->sendbuf_end = 0;
  free(req);
}

void on_wrote_buf(uv_write_t* req, int status) {
  if (status) {
    die("Write error: %s\n", uv_strerror(status));
  }
  peer_state_t* peerstate = (peer_state_t*)req->data;
  peerstate->sendbuf_end = 0;
  free(req);
}

void on_peer_read(uv_stream_t* client, ssize_t nread, const uv_buf_t* buf) {
  if (nread < 0) {
    if (nread != UV_EOF) {
      fprintf(stderr, "Read error: %s\n", uv_strerror(nread));
    }
    // TODO: close connection here? Need to attach the on_handle_closed callback
    // to uv_close and rename it too...
    // uv_close((uv_handle_t*)client, NULL);
    return;
  } else if (nread == 0) {
    // From the documentation of uv_read_cb: nread might be 0, which does not
    // indicate an error or EOF. This is equivalent to EAGAIN or EWOULDBLOCK
    // under read(2).
    return;
  } else {
    // nread > 0
    assert(buf->len >= nread);

    peer_state_t* peerstate = (peer_state_t*)client->data;
    if (peerstate->state == INITIAL_ACK) {
      // If the initial ACK hasn't been sent for some reason, ignore whatever
      // the client sends in.
      free(buf->base);
      return;
    }

    for (int i = 0; i < nread; ++i) {
      switch (peerstate->state) {
      case INITIAL_ACK:
        assert(0 && "can't reach here");
        break;
      case WAIT_FOR_MSG:
        if (buf->base[i] == '^') {
          peerstate->state = IN_MSG;
        }
        break;
      case IN_MSG:
        if (buf->base[i] == '$') {
          peerstate->state = WAIT_FOR_MSG;
        } else {
          assert(peerstate->sendbuf_end < SENDBUF_SIZE);
          peerstate->sendbuf[peerstate->sendbuf_end++] = buf->base[i] + 1;
        }
        break;
      }
    }
    free(buf->base);

    if (peerstate->sendbuf_end > 0) {
      // We have data to send.
      uv_buf_t writebuf =
          uv_buf_init(peerstate->sendbuf, peerstate->sendbuf_end);
      uv_write_t* writereq = (uv_write_t*)xmalloc(sizeof(*writereq));
      writereq->data = peerstate;
      int rc;
      if ((rc = uv_write(writereq, (uv_stream_t*)client, &writebuf, 1,
                         on_wrote_buf)) < 0) {
        die("uv_write failed: %s", uv_strerror(rc));
      }
    }
  }
}

void on_peer_connected(uv_stream_t* server, int status) {
  if (status < 0) {
    fprintf(stderr, "Peer connection error: %s\n", uv_strerror(status));
    return;
  }

  uv_tcp_t* client = (uv_tcp_t*)xmalloc(sizeof(*client));
  int rc;
  if ((rc = uv_tcp_init(uv_default_loop(), client)) < 0) {
    die("uv_tcp_init failed: %s", uv_strerror(rc));
  }

  if (uv_accept(server, (uv_stream_t*)client) == 0) {
    struct sockaddr peername;
    int namelen;

    if ((rc = uv_tcp_getpeername(client, &peername, &namelen)) < 0) {
      die("uv_tcp_getpeername failed: %s", uv_strerror(rc));
    }
    report_peer_connected((const struct sockaddr_in*)&peername, namelen);

    peer_state_t* peerstate = (peer_state_t*)xmalloc(sizeof(*peerstate));
    peerstate->state = INITIAL_ACK;
    peerstate->sendbuf[0] = '*';
    peerstate->sendbuf_end = 1;
    client->data = peerstate;

    uv_buf_t writebuf = uv_buf_init(peerstate->sendbuf, peerstate->sendbuf_end);
    uv_write_t* req = (uv_write_t*)xmalloc(sizeof(*req));
    req->data = peerstate;
    if ((rc = uv_write(req, (uv_stream_t*)client, &writebuf, 1,
                       on_wrote_init_ack)) < 0) {
      die("uv_write failed: %s", uv_strerror(rc));
    }

    if ((rc = uv_read_start((uv_stream_t*)client, on_alloc_buffer,
                            on_peer_read)) < 0) {
      die("uv_read_start failed: %s", uv_strerror(rc));
    }
  } else {
    uv_close((uv_handle_t*)client, NULL);
  }
}

int main(int argc, const char** argv) {
  setvbuf(stdout, NULL, _IONBF, 0);

  int portnum = 9090;
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

  if ((rc = uv_listen((uv_stream_t*)&server, N_BACKLOG, on_peer_connected)) <
      0) {
    die("uv_listen failed: %s", uv_strerror(rc));
  }

  return uv_run(uv_default_loop(), UV_RUN_DEFAULT);
}
