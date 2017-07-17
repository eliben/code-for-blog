#include <assert.h>
#include <errno.h>
#include <stdbool.h>
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

typedef struct {
  bool want_read;
  bool want_write;
} fd_status_t;

const fd_status_t fd_status_R = {.want_read = true, .want_write = false};
const fd_status_t fd_status_W = {.want_read = false, .want_write = true};
const fd_status_t fd_status_RW = {.want_read = true, .want_write = true};
const fd_status_t fd_status_NORW = {.want_read = false, .want_write = false};

void initialize_state(int fd) {
  assert(fd < MAXFDS);
  global_state[fd].state = INITIAL_ACK;
  global_state[fd].sendbuf_end = 0;
  global_state[fd].sendptr = 0;
}

fd_status_t on_connected_peer(int sockfd, const struct sockaddr_in* peer_addr,
                              socklen_t peer_addr_len) {
  report_peer_connected(peer_addr, peer_addr_len);
  initialize_state(sockfd);
  return fd_status_W;
}

fd_status_t on_peer_ready_recv(int sockfd) {
  if (global_state[sockfd].state == INITIAL_ACK ||
      global_state[sockfd].sendptr < global_state[sockfd].sendbuf_end) {
    return fd_status_W;
  }

  char buf[1024];
  int nbytes = recv(sockfd, buf, sizeof buf, 0);
  if (nbytes == 0) {
    return fd_status_NORW;
  } else if (nbytes < 0) {
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      return fd_status_R;
    } else {
      perror_die("recv");
    }
  }
  bool ready_to_send = false;
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
        ready_to_send = true;
      }
      break;
    }
  }
  return (fd_status_t){.want_read = !ready_to_send,
                       .want_write = ready_to_send};
}

fd_status_t on_peer_ready_send(int sockfd) {
  if (global_state[sockfd].state == INITIAL_ACK) {
    int nsent = send(sockfd, "*", 1, 0);
    if (nsent < 1) {
      if (errno == EAGAIN || errno == EWOULDBLOCK) {
        return fd_status_W;
      } else {
        perror_die("send");
      }
    } else {
      global_state[sockfd].state = WAIT_FOR_MSG;
      return fd_status_R;
    }
  }
  if (global_state[sockfd].sendptr >= global_state[sockfd].sendbuf_end) {
    // Nothing to send.
    return fd_status_W;
  }
  int sendlen = global_state[sockfd].sendbuf_end - global_state[sockfd].sendptr;
  int nsent = send(sockfd, global_state[sockfd].sendbuf, sendlen, 0);
  if (nsent == -1) {
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      return fd_status_W;
    } else {
      perror_die("send");
    }
  }
  if (nsent < sendlen) {
    global_state[sockfd].sendptr += nsent;
    return fd_status_W;
  } else {
    global_state[sockfd].sendptr = 0;
    global_state[sockfd].sendbuf_end = 0;
    return fd_status_R;
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

  fd_set readfds_master;
  FD_ZERO(&readfds_master);
  fd_set writefds_master;
  FD_ZERO(&writefds_master);

  if (listener_sockfd >= FD_SETSIZE) {
    die("listener socket fd (%d) >= FD_SETSIZE (%d)", listener_sockfd,
        FD_SETSIZE);
  }

  FD_SET(listener_sockfd, &readfds_master);
  int fdset_max = listener_sockfd;

  while (1) {
    fd_set readfds = readfds_master;
    fd_set writefds = writefds_master;

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
            make_socket_non_blocking(newsockfd);
            if (newsockfd > fdset_max) {
              if (newsockfd >= FD_SETSIZE) {
                die("socket fd (%d) >= FD_SETSIZE (%d)", newsockfd,
                    FD_SETSIZE);
              }
              fdset_max = newsockfd;
            }

            // Notify the user that a new peer connected.
            fd_status_t status =
                on_connected_peer(newsockfd, &peer_addr, peer_addr_len);
            if (status.want_read) {
              FD_SET(newsockfd, &readfds_master);
            } else {
              FD_CLR(newsockfd, &readfds_master);
            }
            if (status.want_write) {
              FD_SET(newsockfd, &writefds_master);
            } else {
              FD_CLR(newsockfd, &writefds_master);
            }
          }
        } else {
          fd_status_t status = on_peer_ready_recv(fd);
          if (status.want_read) {
            FD_SET(fd, &readfds_master);
          } else {
            FD_CLR(fd, &readfds_master);
          }
          if (status.want_write) {
            FD_SET(fd, &writefds_master);
          } else {
            FD_CLR(fd, &writefds_master);
          }
          if (!status.want_read && !status.want_write) {
            printf("socket %d closing\n", fd);
            close(fd);
          }
        }
      }
      if (FD_ISSET(fd, &writefds)) {
        nready--;
        fd_status_t status = on_peer_ready_send(fd);
        if (status.want_read) {
          FD_SET(fd, &readfds_master);
        } else {
          FD_CLR(fd, &readfds_master);
        }
        if (status.want_write) {
          FD_SET(fd, &writefds_master);
        } else {
          FD_CLR(fd, &writefds_master);
        }
        if (!status.want_read && !status.want_write) {
          printf("socket %d closing\n", fd);
          close(fd);
        }
      }
    }
  }

  return 0;
}
