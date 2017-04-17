# Makefile for the C++ code in bfjit.
#
# Some executables here require external libraries (such as asmjit or LLVM); if
# you don't have these available, you can exclude these binaries from the build.
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
	jit_utils_test \
	llvmjit

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

# Point these to asmjit built from source.
# To build the version using asmjit, asmjit needs to be compiled in static mode
# (ASMJIT_STATIC=true). We need the asmjit headers and the statically linked
# archive.
ASMJIT_INCLUDE = -I$(HOME)/test/asmjit/src
ASMJIT_LIB = $(HOME)/test/asmjit/build/libasmjit.a

simpleasmjit: $(COMMONFILES) simpleasmjit.cpp
	$(CXX) $(CXXFLAGS) $(ASMJIT_INCLUDE) $^ $(ASMJIT_LIB) -o $@

optasmjit: $(COMMONFILES) optasmjit.cpp
	$(CXX) $(CXXFLAGS) $(ASMJIT_INCLUDE) $^ $(ASMJIT_LIB) -o $@

# Point these to the binary downoad/build of LLVM/Clang version 4.x
# The LLVM libraries are built with Clang itself (bootstrapped), so the safest
# way to compile programs using LLVM is with the same version of Clang.
LLVM_ROOT = $$HOME/llvm/clang_llvm_rel_40
LLVM_BIN_PATH = $(LLVM_ROOT)/bin
CLANG_CXX = $(LLVM_BIN_PATH)/clang++
LLVM_CXXFLAGS := `$(LLVM_BIN_PATH)/llvm-config --cxxflags`
LLVM_LDFLAGS := `$(LLVM_BIN_PATH)/llvm-config --ldflags --libs --system-libs`

# The rdynamic linker flag is used since this is a JIT loading itself for access
# to host symbols.
llvmjit: utils.cpp parser.cpp llvmjit.cpp
	$(CLANG_CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $^ -rdynamic $(LLVM_LDFLAGS) -o $@

.PHONY: clean format test

test:
	./jit_utils_test
	python3.5 tests/test_bf.py

format:
	clang-format -style=file -i *.cpp *.h

clean:
	rm -f core $(EXECUTABLES) *.o *.gch
