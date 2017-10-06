// Simple blocking socket listener.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#include "utils.h"

int main(int argc, const char** argv) {
  setvbuf(stdout, NULL, _IONBF, 0);

  int portnum = 9988;
  if (argc >= 2) {
    portnum = atoi(argv[1]);
  }
  printf("Listening on port %d\n", portnum);

  int sockfd = listen_inet_socket(portnum);
  struct sockaddr_in peer_addr;
  socklen_t peer_addr_len = sizeof(peer_addr);

  int newsockfd = accept(sockfd, (struct sockaddr*)&peer_addr, &peer_addr_len);
  if (newsockfd < 0) {
    perror_die("ERROR on accept");
  }
  report_peer_connected(&peer_addr, peer_addr_len);

  while (1) {
    uint8_t buf[1024];
    printf("Calling recv...\n");
    int len = recv(newsockfd, buf, sizeof buf, 0);
    if (len < 0) {
      perror_die("recv");
    } else if (len == 0) {
      printf("Peer disconnected; I'm done.\n");
      break;
    }
    printf("recv returned %d bytes\n", len);
  }

  close(newsockfd);
  close(sockfd);

  return 0;
}
