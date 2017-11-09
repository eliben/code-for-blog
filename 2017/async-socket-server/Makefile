# Makefile for the C code in async-socket-server.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CC = gcc
CCFLAGS = -std=gnu99 -Wall -O3 -g -DNDEBUG -pthread
LDFLAGS = -lpthread -pthread

# It's possible to compile uv-server after installing libuv. The full
# instructions for installation I used (including `make install`) are from:
# https://github.com/libuv/libuv/blob/master/README.md.
# libuv compiles into a shared library which is placed alongside the .a in the
# installation directory.
LDLIBUV = -luv -Wl,-rpath=/usr/local/lib

EXECUTABLES = \
	sequential-server \
	select-server \
	epoll-server \
	uv-server \
	uv-timer-sleep-demo \
	uv-timer-work-demo \
	uv-isprime-server \
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

uv-server: utils.c uv-server.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS) $(LDLIBUV)

uv-timer-sleep-demo: utils.c uv-timer-sleep-demo.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS) $(LDLIBUV)

uv-timer-work-demo: utils.c uv-timer-work-demo.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS) $(LDLIBUV)

uv-isprime-server: utils.c uv-isprime-server.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS) $(LDLIBUV)

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
