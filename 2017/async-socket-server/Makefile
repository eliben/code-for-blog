CC = gcc
CCFLAGS = -std=gnu99 -Wall -O3 -g -DNDEBUG -pthread
LDFLAGS = -lpthread -pthread

EXECUTABLES = \
	sequential-server \
	select-server \
	epoll-server \
	threadspammer \
	blocking-listener \
	nonblocking-listener \
	threaded-server

all: $(EXECUTABLES)

sequential-server: utils.c sequential-server.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

select-server: utils.c select-server.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

threaded-server: utils.c threaded-server.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

epoll-server: utils.c epoll-server.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

threadspammer: threadspammer.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

blocking-listener: utils.c blocking-listener.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

nonblocking-listener: utils.c nonblocking-listener.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

.PHONY: clean format

clean:
	rm -f $(EXECUTABLES) *.o

format:
	clang-format -style=file -i *.c *.h
