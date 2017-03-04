# Makefile for the C++ code in bfjit.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

CXX = g++
#CXX = ~/llvm/clang_llvm_rel_39/bin/clang++
CXXFLAGS = -std=c++11 -Wall -O3 -g
DEFINES =
LDFLAGS =

EXECUTABLES = \
	simpleinterp \
	optinterp \
	optinterp2 \
	optinterp3

all: $(EXECUTABLES)

COMMONFILES = utils.cpp utils.h parser.cpp parser.h

simpleinterp: $(COMMONFILES) simpleinterp.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

optinterp: $(COMMONFILES) optinterp.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

optinterp2: $(COMMONFILES) optinterp2.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

optinterp3: $(COMMONFILES) optinterp3.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

.PHONY: clean format test

test:
	python3.5 tests/test_bf.py

format:
	clang-format -style=file -i *.cpp *.h

clean:
	rm -f core $(EXECUTABLES) *.o *.gch
