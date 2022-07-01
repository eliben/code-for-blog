# Makefile to build the sample
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
CC := gcc

CC_FLAGS := -Wall -O3

EXE_TARGETS := \
	interp

all: $(EXE_TARGETS)
.PHONY: all

interp: interp_compute_gotos.c
	$(CC) -o $@ $(CC_FLAGS) $^

clean:
	rm -f a.out *.o *.a $(EXE_TARGETS)

format:
	clang-format -style=file -i *.c

.PHONY: clean format
