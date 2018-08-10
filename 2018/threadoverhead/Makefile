# Makefile for these samples.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CC = gcc
CCFLAGS = -std=gnu99 -Wall -O3 -g -DNDEBUG -pthread

CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG -pthread

LDFLAGS = -lpthread


EXECUTABLES = \
	thread-pipe-msgpersec \
	thread-switch-condvar \
	thread-switch-pipe

all: $(EXECUTABLES)

thread-switch-condvar: thread-switch-condvar.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

thread-switch-pipe: thread-switch-pipe.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

thread-pipe-msgpersec: thread-pipe-msgpersec.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

.PHONY: clean format

clean:
	rm -f $(EXECUTABLES) *.o

format:
	clang-format -style=file -i *.c
