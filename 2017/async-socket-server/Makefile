CC = gcc
CCFLAGS = -std=c99 -Wall -O3 -g -DNDEBUG -pthread
LDFLAGS = -lpthread -pthread

EXECUTABLES = \
	sequential-server

all: $(EXECUTABLES)

sequential-server: sequential-server.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(EXECUTABLES)
