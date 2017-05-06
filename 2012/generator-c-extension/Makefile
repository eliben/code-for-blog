# Makefile for the extension.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CC = gcc
CFLAGS = -g -pthread -fwrapv -Wall -Wno-unused-result -Wstrict-prototypes -fPIC
LDFLAGS = -pthread -shared

# Point PYTHON_DIR to a place where Python headers
PYTHON_DIR = $(HOME)/python-src/Python-3.4.0
INCLUDE = -I$(PYTHON_DIR)/Include -I$(PYTHON_DIR)

all: spam.so

spammodule.o: spammodule.c
	$(CC) $(CFLAGS) $(INCLUDE) -c $^ -o $@

spam.so: spammodule.o
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	rm -rf *.o *.so build dist
