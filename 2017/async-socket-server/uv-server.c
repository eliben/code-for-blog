#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "uv.h"

#include "utils.h"

#define N_BACKLOG 64


void on_peer_connected(uv_stream_t* server, int status) {
  if (status < 0) {
    fprintf(stderr, "Peer connection error: %s\n", uv_strerror(status));
    return;
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

  if ((rc = uv_listen((uv_stream_t*)&server, N_BACKLOG,
                      on_peer_connected)) < 0) {
    die("uv_listen failed: %s", uv_strerror(rc));
  }

  return uv_run(uv_default_loop(), UV_RUN_DEFAULT);
}
