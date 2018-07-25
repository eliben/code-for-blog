# Makefile for these samples.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CC = gcc
CCFLAGS = -std=gnu99 -Wall -O3 -g -DNDEBUG

LDFLAGS = -pthread

EXECUTABLES = \
	clone-vm-sample \
	launch-benchmark

all: $(EXECUTABLES)

clone-vm-sample: clone-vm-sample.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

launch-benchmark: launch-benchmark.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

.PHONY: clean format

clean:
	rm -f $(EXECUTABLES) *.o

format:
	clang-format -style=file -i *.c
