#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "utils.h"

int main(int argc, char** argv) {
  setvbuf(stdout, NULL, _IONBF, 0);

  int portnum = 9090;
  if (argc >= 2) {
    portnum = atoi(argv[1]);
  }
  printf("Serving on port %d\n", portnum);

  int listener_sockfd = listen_inet_socket(portnum);

  // TODO: comment here why
  make_socket_non_blocking(listener_sockfd);

  fd_set select_fdset;
  FD_ZERO(&select_fdset);

  if (listener_sockfd >= FD_SETSIZE) {
    die("listener socket fd (%d) >= FD_SETSIZE (%d)\n", listener_sockfd,
        FD_SETSIZE);
  }

  FD_SET(listener_sockfd, &select_fdset);
  int fdset_max = listener_sockfd;

  while (1) {
    // Since select modifies the fd set in place to indicate ready fds, we pass
    // a copy to keep the original fdset unchanged.
    fd_set select_fdset_ready = select_fdset;

    int nready = select(fdset_max + 1, &select_fdset_ready, NULL, NULL, NULL);
    if (nready < 0) {
      perror_die("select");
    }

    for (int fd = 0; fd <= fdset_max && nready > 0; ++fd) {
      if (FD_ISSET(fd, &select_fdset_ready)) {
        nready--;

        if (fd == listener_sockfd) {
          // The listening socket is ready; this means a new peer is
          // connecting.
          struct sockaddr_in peer_addr;
          socklen_t peer_addr_len = sizeof(peer_addr);
          int newsockfd = accept(listener_sockfd, (struct sockaddr*)&peer_addr,
              &peer_addr_len);
          if (newsockfd < 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
              // This can happen due to the nonblocking socket mode; in this
              // case don't do anything, but print a notice (since these events
              // are extremely rare and interesting to observe...)
              printf("accept returned EAGAIN or EWOULDBLOCK\n");
            } else {
              perror_die("accept");
            }
          } else {
            // newsockfd is a valid new fd for a peer. Make it nonblocking and
            // add it to the main select set, so that we'll wait for it to
            // become ready in the next iteration of this select loop.
            make_socket_non_blocking(newsockfd);
            FD_SET(newsockfd, &select_fdset);
            if (newsockfd > fdset_max) {
              if (newsockfd >= FD_SETSIZE) {
                die("socket fd (%d) >= FD_SETSIZE (%d)\n", newsockfd,
                    FD_SETSIZE);
              }
              fdset_max = newsockfd;
            }
            report_peer_connected(&peer_addr, peer_addr_len);
            // TODO: send the starting * to the client here. Simplification:
            // assuming socket is ready for send; if i get EAGAIN etc. it's an
            // error
            if (send(newsockfd, "*", 1, 0) < 1) {
              perror_die("send");
            }
          }
        } else {
          // One of the peer sockets is ready to receive data.
          char buf[1024];
          int nbytes = recv(fd, buf, sizeof buf, 0);
          if (nbytes <= 0) {
            if (nbytes == 0) {
              printf("socket %d hung up\n", fd);
              close(fd);
              FD_CLR(fd, &select_fdset);
            } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
              printf("recv returned EAGAIN or EWOULDBLOCK\n");
            } else {
              perror_die("recv");
            }
          } else {
            // Got data from the client
            printf("got data from %d: ", fd);
            for (int i = 0; i < nbytes; ++i) {
              printf("0x%x ", (unsigned)buf[i]);
            }
            printf("\n");
          }
        }
      }
    }
  }

  return 0;
}
