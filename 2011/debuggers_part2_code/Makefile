# Makefile for these samples.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CC = gcc
CCFLAGS = -std=gnu99 -Wall -O0 -no-pie

LDFLAGS = -L. -ldebug

EXECUTABLES = \
	bp_manual \
	bp_use_lib \
	traced_c_loop

ADDR = `objdump -d traced_c_loop | grep do_stuff | head -n 1 | awk '{print $1;}'`

.PHONY: all clean

all: $(EXECUTABLES)

libdebug.a: debuglib.c debuglib.h
	$(CC) $(CCFLAGS) -O -c debuglib.c
	ar rcs libdebug.a debuglib.o

bp_manual: bp_manual.c libdebug.a
	$(CC) $(CCFLAGS) $< -o $@ $(LDFLAGS)

test_bp_manual: bp_manual traced_c_loop
	./bp_manual traced_c_loop 0x$(ADDR)

bp_use_lib: bp_use_lib.c libdebug.a
	$(CC) $(CCFLAGS) $< -o $@ $(LDFLAGS)

test_bp_use_lib: bp_use_lib traced_c_loop
	./bp_use_lib traced_c_loop 0x$(ADDR)

traced_c_loop: traced_c_loop.c
	$(CC) $(CCFLAGS) $^ -o $@

clean:
	rm -f $(EXECUTABLES) *.o *.a
