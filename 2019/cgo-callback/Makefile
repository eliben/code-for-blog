CC := gcc
CFLAGS := -Wall -std=c99 -g -O0

EXE_TARGETS := \
	libclibrary.a

all: $(EXE_TARGETS)
.PHONY: all

libclibrary.a: clibrary.c clibrary.h
	$(CC) $(CFLAGS) -c clibrary.c
	ar cr $@ clibrary.o

clean:
	rm -f a.out *.o *.a $(EXE_TARGETS)

format:
	clang-format -style=file -i *.c

.PHONY: clean format
