#ifndef UTILS_H
#define UTILS_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

void perror_die(char* msg);

void report_peer_name(char* peername, size_t peernamelen,
                      struct sockaddr_in* sa, socklen_t salen);

int listen_inet_socket(int portnum);

#endif /* UTILS_H */
