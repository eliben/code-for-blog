// Asynchronous socket server - accepting multiple clients concurrently,
// using libuv's event loop.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "uv.h"

#include "utils.h"

#define N_BACKLOG 64

typedef enum { INITIAL_ACK, WAIT_FOR_MSG, IN_MSG } ProcessingState;

#define SENDBUF_SIZE 1024

typedef struct {
  ProcessingState state;
  char sendbuf[SENDBUF_SIZE];
  int sendbuf_end;
  uv_tcp_t* client;
} peer_state_t;

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

void on_wrote_buf(uv_write_t* req, int status) {
  if (status) {
    die("Write error: %s\n", uv_strerror(status));
  }
  peer_state_t* peerstate = (peer_state_t*)req->data;

  // Kill switch for testing leaks in the server. When a client sends a message
  // ending with WXY (note the shift-by-1 in sendbuf), this signals the server
  // to clean up and exit, by stopping the default event loop. Running the
  // server under valgrind can now track memory leaks, and a run should be
  // clean except a single uv_tcp_t allocated for the client that sent the kill
  // signal (it's still connected when we stop the loop and exit).
  if (peerstate->sendbuf_end >= 3 &&
      peerstate->sendbuf[peerstate->sendbuf_end - 3] == 'X' &&
      peerstate->sendbuf[peerstate->sendbuf_end - 2] == 'Y' &&
      peerstate->sendbuf[peerstate->sendbuf_end - 1] == 'Z') {
    free(peerstate);
    free(req);
    uv_stop(uv_default_loop());
    return;
  }

  // The send buffer is done; move pointer back to 0.
  peerstate->sendbuf_end = 0;
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

    peer_state_t* peerstate = (peer_state_t*)client->data;
    if (peerstate->state == INITIAL_ACK) {
      // If the initial ACK hasn't been sent for some reason, ignore whatever
      // the client sends in.
      free(buf->base);
      return;
    }

    // Run the protocol state machine.
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

    if (peerstate->sendbuf_end > 0) {
      // We have data to send. The write buffer will point to the buffer stored
      // in the peer state for this client.
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
  free(buf->base);
}

void on_wrote_init_ack(uv_write_t* req, int status) {
  if (status) {
    die("Write error: %s\n", uv_strerror(status));
  }
  peer_state_t* peerstate = (peer_state_t*)req->data;
  // Flip the peer state to WAIT_FOR_MSG, and start listening for incoming data
  // from this peer.
  peerstate->state = WAIT_FOR_MSG;
  peerstate->sendbuf_end = 0;

  int rc;
  if ((rc = uv_read_start((uv_stream_t*)peerstate->client, on_alloc_buffer,
                          on_peer_read)) < 0) {
    die("uv_read_start failed: %s", uv_strerror(rc));
  }

  // Note: the write request doesn't own the peer state, hence we only free the
  // request itself, not the state.
  free(req);
}

void on_peer_connected(uv_stream_t* server_stream, int status) {
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

  if (uv_accept(server_stream, (uv_stream_t*)client) == 0) {
    struct sockaddr_storage peername;
    int namelen = sizeof(peername);
    if ((rc = uv_tcp_getpeername(client, (struct sockaddr*)&peername,
                                 &namelen)) < 0) {
      die("uv_tcp_getpeername failed: %s", uv_strerror(rc));
    }
    report_peer_connected((const struct sockaddr_in*)&peername, namelen);

    // Initialize the peer state for a new client: we start by sending the peer
    // the initial '*' ack.
    peer_state_t* peerstate = (peer_state_t*)xmalloc(sizeof(*peerstate));
    peerstate->state = INITIAL_ACK;
    peerstate->sendbuf[0] = '*';
    peerstate->sendbuf_end = 1;
    peerstate->client = client;
    client->data = peerstate;

    // Enqueue the write request to send the ack; when it's done,
    // on_wrote_init_ack will be called. The peer state is passed to the write
    // request via the data pointer; the write request does not own this peer
    // state - it's owned by the client handle.
    uv_buf_t writebuf = uv_buf_init(peerstate->sendbuf, peerstate->sendbuf_end);
    uv_write_t* req = (uv_write_t*)xmalloc(sizeof(*req));
    req->data = peerstate;
    if ((rc = uv_write(req, (uv_stream_t*)client, &writebuf, 1,
                       on_wrote_init_ack)) < 0) {
      die("uv_write failed: %s", uv_strerror(rc));
    }
  } else {
    uv_close((uv_handle_t*)client, on_client_closed);
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
  uv_tcp_t server_stream;
  if ((rc = uv_tcp_init(uv_default_loop(), &server_stream)) < 0) {
    die("uv_tcp_init failed: %s", uv_strerror(rc));
  }

  struct sockaddr_in server_address;
  if ((rc = uv_ip4_addr("0.0.0.0", portnum, &server_address)) < 0) {
    die("uv_ip4_addr failed: %s", uv_strerror(rc));
  }

  if ((rc = uv_tcp_bind(&server_stream, (const struct sockaddr*)&server_address,
                        0)) < 0) {
    die("uv_tcp_bind failed: %s", uv_strerror(rc));
  }

  // Listen on the socket for new peers to connect. When a new peer connects,
  // the on_peer_connected callback will be invoked.
  if ((rc = uv_listen((uv_stream_t*)&server_stream, N_BACKLOG,
                      on_peer_connected)) < 0) {
    die("uv_listen failed: %s", uv_strerror(rc));
  }

  // Run the libuv event loop.
  uv_run(uv_default_loop(), UV_RUN_DEFAULT);

  // If uv_run returned, close the default loop before exiting.
  return uv_loop_close(uv_default_loop());
}
