// Asynchronous socket server - accepting multiple clients concurrently,
// multiplexing the connections with epoll.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "utils.h"

#define MAXFDS 16 * 1024

typedef enum { INITIAL_ACK, WAIT_FOR_MSG, IN_MSG } ProcessingState;

#define SENDBUF_SIZE 1024

typedef struct {
  ProcessingState state;
  uint8_t sendbuf[SENDBUF_SIZE];
  int sendbuf_end;
  int sendptr;
} peer_state_t;

// Each peer is globally identified by the file descriptor (fd) it's connected
// on. As long as the peer is connected, the fd is unique to it. When a peer
// disconnects, a new peer may connect and get the same fd. on_peer_connected
// should initialize the state properly to remove any trace of the old peer on
// the same fd.
peer_state_t global_state[MAXFDS];

// Callbacks (on_XXX functions) return this status to the main loop; the status
// instructs the loop about the next steps for the fd for which the callback was
// invoked.
// want_read=true means we want to keep monitoring this fd for reading.
// want_write=true means we want to keep monitoring this fd for writing.
// When both are false it means the fd is no longer needed and can be closed.
typedef struct {
  bool want_read;
  bool want_write;
} fd_status_t;

// These constants make creating fd_status_t values less verbose.
const fd_status_t fd_status_R = {.want_read = true, .want_write = false};
const fd_status_t fd_status_W = {.want_read = false, .want_write = true};
const fd_status_t fd_status_RW = {.want_read = true, .want_write = true};
const fd_status_t fd_status_NORW = {.want_read = false, .want_write = false};

fd_status_t on_peer_connected(int sockfd, const struct sockaddr_in* peer_addr,
                              socklen_t peer_addr_len) {
  assert(sockfd < MAXFDS);
  report_peer_connected(peer_addr, peer_addr_len);

  // Initialize state to send back a '*' to the peer immediately.
  peer_state_t* peerstate = &global_state[sockfd];
  peerstate->state = INITIAL_ACK;
  peerstate->sendbuf[0] = '*';
  peerstate->sendptr = 0;
  peerstate->sendbuf_end = 1;

  // Signal that this socket is ready for writing now.
  return fd_status_W;
}

fd_status_t on_peer_ready_recv(int sockfd) {
  assert(sockfd < MAXFDS);
  peer_state_t* peerstate = &global_state[sockfd];

  if (peerstate->state == INITIAL_ACK ||
      peerstate->sendptr < peerstate->sendbuf_end) {
    // Until the initial ACK has been sent to the peer, there's nothing we
    // want to receive. Also, wait until all data staged for sending is sent to
    // receive more data.
    return fd_status_W;
  }

  uint8_t buf[1024];
  int nbytes = recv(sockfd, buf, sizeof buf, 0);
  if (nbytes == 0) {
    // The peer disconnected.
    return fd_status_NORW;
  } else if (nbytes < 0) {
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      // The socket is not *really* ready for recv; wait until it is.
      return fd_status_R;
    } else {
      perror_die("recv");
    }
  }
  bool ready_to_send = false;
  for (int i = 0; i < nbytes; ++i) {
    switch (peerstate->state) {
    case INITIAL_ACK:
      assert(0 && "can't reach here");
      break;
    case WAIT_FOR_MSG:
      if (buf[i] == '^') {
        peerstate->state = IN_MSG;
      }
      break;
    case IN_MSG:
      if (buf[i] == '$') {
        peerstate->state = WAIT_FOR_MSG;
      } else {
        assert(peerstate->sendbuf_end < SENDBUF_SIZE);
        peerstate->sendbuf[peerstate->sendbuf_end++] = buf[i] + 1;
        ready_to_send = true;
      }
      break;
    }
  }
  // Report reading readiness iff there's nothing to send to the peer as a
  // result of the latest recv.
  return (fd_status_t){.want_read = !ready_to_send,
                       .want_write = ready_to_send};
}

fd_status_t on_peer_ready_send(int sockfd) {
  assert(sockfd < MAXFDS);
  peer_state_t* peerstate = &global_state[sockfd];

  if (peerstate->sendptr >= peerstate->sendbuf_end) {
    // Nothing to send.
    return fd_status_RW;
  }
  int sendlen = peerstate->sendbuf_end - peerstate->sendptr;
  int nsent = send(sockfd, &peerstate->sendbuf[peerstate->sendptr], sendlen, 0);
  if (nsent == -1) {
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      return fd_status_W;
    } else {
      perror_die("send");
    }
  }
  if (nsent < sendlen) {
    peerstate->sendptr += nsent;
    return fd_status_W;
  } else {
    // Everything was sent successfully; reset the send queue.
    peerstate->sendptr = 0;
    peerstate->sendbuf_end = 0;

    // Special-case state transition in if we were in INITIAL_ACK until now.
    if (peerstate->state == INITIAL_ACK) {
      peerstate->state = WAIT_FOR_MSG;
    }

    return fd_status_R;
  }
}

int main(int argc, const char** argv) {
  setvbuf(stdout, NULL, _IONBF, 0);

  int portnum = 9090;
  if (argc >= 2) {
    portnum = atoi(argv[1]);
  }
  printf("Serving on port %d\n", portnum);

  int listener_sockfd = listen_inet_socket(portnum);
  make_socket_non_blocking(listener_sockfd);

  int epollfd = epoll_create1(0);
  if (epollfd < 0) {
    perror_die("epoll_create1");
  }

  struct epoll_event accept_event;
  accept_event.data.fd = listener_sockfd;
  accept_event.events = EPOLLIN;
  if (epoll_ctl(epollfd, EPOLL_CTL_ADD, listener_sockfd, &accept_event) < 0) {
    perror_die("epoll_ctl EPOLL_CTL_ADD");
  }

  struct epoll_event* events = calloc(MAXFDS, sizeof(struct epoll_event));
  if (events == NULL) {
    die("Unable to allocate memory for epoll_events");
  }

  while (1) {
    int nready = epoll_wait(epollfd, events, MAXFDS, -1);
    for (int i = 0; i < nready; i++) {
      if (events[i].events & EPOLLERR) {
        perror_die("epoll_wait returned EPOLLERR");
      }

      if (events[i].data.fd == listener_sockfd) {
        // The listening socket is ready; this means a new peer is connecting.

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
          make_socket_non_blocking(newsockfd);
          if (newsockfd >= MAXFDS) {
            die("socket fd (%d) >= MAXFDS (%d)", newsockfd, MAXFDS);
          }

          fd_status_t status =
              on_peer_connected(newsockfd, &peer_addr, peer_addr_len);
          struct epoll_event event = {0};
          event.data.fd = newsockfd;
          if (status.want_read) {
            event.events |= EPOLLIN;
          }
          if (status.want_write) {
            event.events |= EPOLLOUT;
          }

          if (epoll_ctl(epollfd, EPOLL_CTL_ADD, newsockfd, &event) < 0) {
            perror_die("epoll_ctl EPOLL_CTL_ADD");
          }
        }
      } else {
        // A peer socket is ready.
        if (events[i].events & EPOLLIN) {
          // Ready for reading.
          int fd = events[i].data.fd;
          fd_status_t status = on_peer_ready_recv(fd);
          struct epoll_event event = {0};
          event.data.fd = fd;
          if (status.want_read) {
            event.events |= EPOLLIN;
          }
          if (status.want_write) {
            event.events |= EPOLLOUT;
          }
          if (event.events == 0) {
            printf("socket %d closing\n", fd);
            if (epoll_ctl(epollfd, EPOLL_CTL_DEL, fd, NULL) < 0) {
              perror_die("epoll_ctl EPOLL_CTL_DEL");
            }
            close(fd);
          } else if (epoll_ctl(epollfd, EPOLL_CTL_MOD, fd, &event) < 0) {
            perror_die("epoll_ctl EPOLL_CTL_MOD");
          }
        } else if (events[i].events & EPOLLOUT) {
          // Ready for writing.
          int fd = events[i].data.fd;
          fd_status_t status = on_peer_ready_send(fd);
          struct epoll_event event = {0};
          event.data.fd = fd;

          if (status.want_read) {
            event.events |= EPOLLIN;
          }
          if (status.want_write) {
            event.events |= EPOLLOUT;
          }
          if (event.events == 0) {
            printf("socket %d closing\n", fd);
            if (epoll_ctl(epollfd, EPOLL_CTL_DEL, fd, NULL) < 0) {
              perror_die("epoll_ctl EPOLL_CTL_DEL");
            }
            close(fd);
          } else if (epoll_ctl(epollfd, EPOLL_CTL_MOD, fd, &event) < 0) {
            perror_die("epoll_ctl EPOLL_CTL_MOD");
          }
        }
      }
    }
  }

  return 0;
}
