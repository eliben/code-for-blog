#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#define _GNU_SOURCE
#include <netdb.h>
#include <unistd.h>

#define N_BACKLOG 15

void perror_die(char* msg) {
  perror(msg);
  exit(1);
}

typedef enum {
  WAIT_FOR_START,
  START,
  IN_MSG,
  END
} ProcessingState;

void serve_connection(int sockfd) {
  ProcessingState state = WAIT_FOR_START;

  while (1) {
    char buf[1024];
    int len = recv(sockfd, buf, sizeof buf, 0);
    if (len < 0) {
      perror_die("recv");
    } else if (len == 0) {
      break;
    }

    for (int i = 0; i < len; ++i) {
      switch (state) {
        case WAIT_FOR_START:
          if (buf[i] == '^') {
            state = START;
          }
          break;
        case START:
          state = IN_MSG;
          break;
        case IN_MSG:
          if (buf[i] == '$') {
            state = END;
          } else {
            buf[i] += 1;
            if (send(sockfd, &buf[i], 1, 0) < 1) {
              perror("send error");
              close(sockfd);
              return;
            }
          }
          break;
        case END:
          state = WAIT_FOR_START;
          break;
      }
    }
  }

  close(sockfd);
}

void report_peer_name(char* peername, size_t peernamelen,
                      struct sockaddr_in* sa, socklen_t salen) {
  char hostbuf[NI_MAXHOST];
  char portbuf[NI_MAXSERV];
  if (getnameinfo((struct sockaddr*)sa, salen, hostbuf, NI_MAXHOST, portbuf,
                  NI_MAXSERV, 0) == 0) {
    snprintf(peername, peernamelen, "(%s, %s)", hostbuf, portbuf);
  } else {
    snprintf(peername, peernamelen, "(UNKNOWN)");
  }
}

int main(int argc, char** argv) {
  int portnum = 9090;
  if (argc >= 2) {
    portnum = atoi(argv[1]);
  }
  printf("Serving on port %d\n", portnum);

  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    perror_die("ERROR opening socket");
  }

  struct sockaddr_in serv_addr;
  memset(&serv_addr, 0, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = INADDR_ANY;
  serv_addr.sin_port = htons(portnum);

  if (bind(sockfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr)) < 0) {
    perror_die("ERROR on binding");
  }

  if (listen(sockfd, N_BACKLOG) < 0) {
    perror_die("ERROR on listen");
  }

  while (1) {
    struct sockaddr_in peer_addr;
    socklen_t peer_addr_len = sizeof(peer_addr);
    char peername[1024];

    int newsockfd =
        accept(sockfd, (struct sockaddr*)&peer_addr, &peer_addr_len);

    if (newsockfd < 0) {
      perror_die("ERROR on accept");
    }

    report_peer_name(peername, 1024, &peer_addr, peer_addr_len);
    printf("%s connected\n", peername);

    serve_connection(newsockfd);
    printf("%s done\n", peername);
  }

  return 0;
}
