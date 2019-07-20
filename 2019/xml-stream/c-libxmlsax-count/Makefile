# Makefile to compile a sample linked with libxml
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CC := gcc
LIBXML_CC_FLAGS := -std=c99 -Wall -g `xml2-config --cflags`
LIBXML_LD_FLAGS := `xml2-config --libs`

EXE_TARGETS := \
	c-libxmlsax-count

all: $(EXE_TARGETS)
.PHONY: all

c-libxmlsax-count: c-libxmlsax-count.c
	$(CC) -o $@ $(LIBXML_CC_FLAGS) $^ $(LIBXML_LD_FLAGS)

clean:
	rm -f a.out *.o *.a $(EXE_TARGETS)

format:
	clang-format -style=file -i *.c

.PHONY: clean format
