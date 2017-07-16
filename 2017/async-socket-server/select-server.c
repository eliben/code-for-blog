#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "utils.h"

// Note: FD_SETSIZE is 1024 on Linux
#define MAXFDS 1000

typedef enum { INITIAL_ACK, WAIT_FOR_MSG, IN_MSG } ProcessingState;

typedef struct {
  ProcessingState state;
  char sendbuf[1024];
  int sendbuf_end;
  int sendptr;
} peer_state_t;

peer_state_t global_state[MAXFDS];

void initialize_state(int fd) {
  assert(fd < MAXFDS);
  global_state[fd].state = INITIAL_ACK;
  global_state[fd].sendbuf_end = 0;
  global_state[fd].sendptr = 0;
}

void on_connected_peer(int sockfd, const struct sockaddr_in* peer_addr,
                       socklen_t peer_addr_len) {
  report_peer_connected(peer_addr, peer_addr_len);
  initialize_state(sockfd);
}

// TODO: returns recv's rc if rc <= 0, otherwise 1.
int on_peer_ready_recv(int sockfd) {
  if (global_state[sockfd].state == INITIAL_ACK) {
    return 1;
  }
  if (global_state[sockfd].sendptr < global_state[sockfd].sendbuf_end) {
    // There's still data remaining to be sent back; don't receive new data for
    // now.
    return 1;
  }

  char buf[1024];
  int nbytes = recv(sockfd, buf, sizeof buf, 0);
  if (nbytes <= 0) {
    return nbytes;
  } else {
    for (int i = 0; i < nbytes; ++i) {
      switch (global_state[sockfd].state) {
        case INITIAL_ACK:
          assert(0 && "can't reach here");
          break;
      case WAIT_FOR_MSG:
        if (buf[i] == '^') {
          global_state[sockfd].state = IN_MSG;
        }
        break;
      case IN_MSG:
        if (buf[i] == '$') {
          global_state[sockfd].state = WAIT_FOR_MSG;
        } else {
          global_state[sockfd].sendbuf[global_state[sockfd].sendbuf_end++] =
              buf[i] + 1;
        }
        break;
      }
    }
  }
  return 1;
}

void on_peer_ready_send(int sockfd) {
  if (global_state[sockfd].state == INITIAL_ACK) {
    int nsent = send(sockfd, "*", 1, 0);
    if (nsent < 1) {
      if (errno == EAGAIN || errno == EWOULDBLOCK) {
        // Can't really send right now; state remains unchanged.
        return;
      } else {
        perror_die("send");
      }
    } else {
      global_state[sockfd].state = WAIT_FOR_MSG;
    }
  } else {
    if (global_state[sockfd].sendptr < global_state[sockfd].sendbuf_end) {
      int sendlen = global_state[sockfd].sendbuf_end - global_state[sockfd].sendptr;
      int nsent = send(sockfd, global_state[sockfd].sendbuf, sendlen, 0);
      if (nsent == -1) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
          return;
        } else {
          perror_die("send");
        }
      }
      if (nsent < sendlen) {
        global_state[sockfd].sendptr += nsent;
      } else {
        global_state[sockfd].sendptr = 0;
        global_state[sockfd].sendbuf_end = 0;
      }
    }
  }
}

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

  // TODO: need separate master for reading and writing.
  fd_set master_fdset;
  FD_ZERO(&master_fdset);

  if (listener_sockfd >= FD_SETSIZE) {
    die("listener socket fd (%d) >= FD_SETSIZE (%d)", listener_sockfd,
        FD_SETSIZE);
  }

  FD_SET(listener_sockfd, &master_fdset);
  int fdset_max = listener_sockfd;

  while (1) {
    fd_set readfds = master_fdset;
    fd_set writefds = master_fdset;

    int nready = select(fdset_max + 1, &readfds, &writefds, NULL, NULL);
    if (nready < 0) {
      perror_die("select");
    }

    for (int fd = 0; fd <= fdset_max && nready > 0; ++fd) {
      if (FD_ISSET(fd, &readfds)) {
        nready--;

        if (fd == listener_sockfd) {
          // The listening socket is ready; this means a new peer is
          // connecting.
          struct sockaddr_in peer_addr;
          socklen_t peer_addr_len = sizeof(peer_addr);
          int newsockfd = accept(listener_sockfd, (struct sockaddr*)&peer_addr,
                                 &peer_addr_len);
          // shutdown here if > MAXFDs?
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
            FD_SET(newsockfd, &master_fdset);
            if (newsockfd > fdset_max) {
              if (newsockfd >= FD_SETSIZE) {
                die("socket fd (%d) >= FD_SETSIZE (%d)", newsockfd,
                    FD_SETSIZE);
              }
              fdset_max = newsockfd;
            }

            // Notify the user that a new peer connected.
            on_connected_peer(newsockfd, &peer_addr, peer_addr_len);
          }
        } else {
          int rc = on_peer_ready_recv(fd);
          if (rc == 0) {
            printf("socket %d hung up\n", fd);
            close(fd);
            FD_CLR(fd, &master_fdset);
          } else if (rc < 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
              printf("recv returned EAGAIN or EWOULDBLOCK\n");
            } else {
              perror_die("recv");
            }
          }
        }
      }
      if (FD_ISSET(fd, &writefds)) {
        nready--;
        on_peer_ready_send(fd);
      }
    }
  }

  return 0;
}
