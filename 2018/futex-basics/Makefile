# Makefile for the C/C++ code in async-socket-server.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CC = gcc
CCFLAGS = -std=gnu99 -Wall -O3 -g -DNDEBUG -pthread

CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG -pthread

LDFLAGS = -lpthread


EXECUTABLES = \
	futex-basic-process \
	futex-basic-thread \
	futex-wait-timeout \
	mutex-using-futex

all: $(EXECUTABLES)

futex-basic-process: futex-basic-process.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

futex-basic-thread: futex-basic-thread.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

futex-wait-timeout: futex-wait-timeout.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

mutex-using-futex: mutex-using-futex.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

.PHONY: clean format

clean:
	rm -f $(EXECUTABLES) *.o

format:
	clang-format -style=file -i *.c *.cpp
