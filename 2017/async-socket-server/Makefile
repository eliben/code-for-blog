CC = gcc
CCFLAGS = -std=gnu99 -Wall -O3 -g -DNDEBUG -pthread
LDFLAGS = -lpthread -pthread

EXECUTABLES = \
	sequential-server

all: $(EXECUTABLES)

sequential-server: utils.c sequential-server.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(EXECUTABLES) *.o
