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
	optinterp3 \
	simplejit \
	simpleasmjit \
	jit_utils_test

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

jit_utils_test: $(COMMONFILES) jit_utils_test.cpp jit_utils.cpp jit_utils.h 
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

simplejit: $(COMMONFILES) simplejit.cpp jit_utils.h jit_utils.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

# To build the version using asmjit, asmjit needs to be compiled in static mode
# (ASMJIT_STATIC=true). We need the asmjit headers and the statically linked
# archive.
ASMJIT_INCLUDE = -I/home/eliben/test/asmjit/src
ASMJIT_LIB = /home/eliben/test/asmjit/build/libasmjit.a

simpleasmjit: $(COMMONFILES) simpleasmjit.cpp
	$(CXX) $(CXXFLAGS) $(ASMJIT_INCLUDE) $^ $(ASMJIT_LIB) -o $@


.PHONY: clean format test

test:
	./jit_utils_test
	python3.5 tests/test_bf.py

format:
	clang-format -style=file -i *.cpp *.h

clean:
	rm -f core $(EXECUTABLES) *.o *.gch
