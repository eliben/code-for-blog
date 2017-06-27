#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>

#define N_BACKLOG 15

void perror_die(char* msg) {
  perror(msg);
  exit(1);
}

void serve_connection(int sockfd) {
  char buf[1024];
  int cc;

  while ((cc = recv(sockfd, buf, sizeof buf, 0)) > 0) {
    if (send(sockfd, "echo:", 6, 0) < 1 || send(sockfd, buf, cc, 0) < 1) {
      perror("SEND error");
      break;
    }
  }

  close(sockfd);
}

int main(int argc, char** argv) {
  int sockfd, newsockfd, portno;
  char buffer[1025];
  char service[32];
  struct sockaddr_in serv_addr;

  if (argc < 2) {
    fprintf(stderr, "ERROR, no port provided\n");
    exit(1);
  }

  sockfd = socket(AF_INET, SOCK_STREAM, 0);

  if (sockfd < 0) {
    perror_die("ERROR opening socket");
  }

  memset(&serv_addr, 0, sizeof(serv_addr));

  portno = atoi(argv[1]);

  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = INADDR_ANY;
  serv_addr.sin_port = htons(portno);

  if (bind(sockfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr)) < 0) {
    perror_die("ERROR on binding");
  }

  if (listen(sockfd, N_BACKLOG) < 0) {
    perror_die("ERROR on listen");
  }

  while (1) {
    struct sockaddr_in peer_addr;
    socklen_t peer_addr_len = sizeof(peer_addr);

    newsockfd = accept(sockfd, (struct sockaddr*)&peer_addr, &peer_addr_len);

    if (newsockfd < 0) {
      perror_die("ERROR on accept");
    }

    if (getnameinfo((struct sockaddr*)&peer_addr, peer_addr_len, buffer, 1025,
                    service, 32, 0) == 0) {
      printf("(%s, %s)\n", buffer, service);
    }
    else {
      printf("(?UNKNOWN?)\n");
    }

    serve_connection(newsockfd);
  }

  return 0;
}
