# Makefile for the C++ code in bfjit.
#
# Some executables here require external libraries (such as asmjit); if you
# don't have these available, you can exclude these binaries from the build.
# Note that the Python test script (test_bf) tries to run all executables, so
# some of its tests will fail.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

CXX = g++
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
	optasmjit \
	jit_utils_test

all: $(EXECUTABLES)

COMMONFILES = utils.o parser.o

jit_utils.o: jit_utils.h utils.h jit_utils.cpp
jit_utils_test.o: jit_utils.h jit_utils_test.cpp
optasmjit.o: parser.h utils.h optasmjit.cpp
optinterp.o: parser.h utils.h optinterp.cpp
optinterp2.o: parser.h utils.h optinterp2.cpp
optinterp3.o: parser.h utils.h optinterp3.cpp
parser.o: parser.h utils.h parser.cpp
simpleasmjit.o: parser.h utils.h simpleasmjit.cpp
simpleinterp.o: parser.h utils.h simpleinterp.cpp
simplejit.o: jit_utils.h parser.h utils.h simplejit.cpp
utils.o: utils.h utils.cpp

simpleinterp: simpleinterp.o $(COMMONFILES)
	$(CXX) $(CXXFLAGS) $* $(LDFLAGS) $^ -o $@

optinterp: optinterp.cpp $(COMMONFILES)
	$(CXX) $(CXXFLAGS) $* $(LDFLAGS) $^ -o $@

optinterp2: optinterp2.cpp $(COMMONFILES)
	$(CXX) $(CXXFLAGS) $* $(LDFLAGS) $^ -o $@

optinterp3: optinterp3.cpp $(COMMONFILES)
	$(CXX) $(CXXFLAGS) $* $(LDFLAGS) $^ -o $@

jit_utils_test: jit_utils_test.o jit_utils.o $(COMMONFILES)
	$(CXX) $(CXXFLAGS) $* $(LDFLAGS) $^ -o $@

simplejit: simplejit.o jit_utils.o $(COMMONFILES)
	$(CXX) $(CXXFLAGS) $* $(LDFLAGS) $^ -o $@

# Point these to asmjit built from source.
# To build the version using asmjit, asmjit needs to be compiled in static mode
# (ASMJIT_STATIC=true). We need the asmjit headers and the statically linked
# archive.
ASMJIT_HOME?=/home/eliben/test/asmjit/
ASMJIT_INCLUDE = -I$(ASMJIT_HOME)/src
ASMJIT_LIB = $(ASMJIT_HOME)/build/libasmjit.a

simpleasmjit: $(COMMONFILES) simpleasmjit.cpp
	$(CXX) $(CXXFLAGS) $(ASMJIT_INCLUDE) $^ $(ASMJIT_LIB) -o $@

optasmjit: $(COMMONFILES) optasmjit.cpp
	$(CXX) $(CXXFLAGS) $(ASMJIT_INCLUDE) $^ $(ASMJIT_LIB) -o $@

.PHONY: clean format test

test:
	./jit_utils_test
	python3.5 tests/test_bf.py

format:
	clang-format -style=file -i *.cpp *.h

clean:
	rm -f core $(EXECUTABLES) *.o *.gch
