# Makefile for covar samples.
# #
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
CXX := g++
CXXFLAGS := -Wall -g -std=c++14 -O3
LDFLAGS :=

# Keep all the first target so it's the default.

EXE_TARGETS := \
	arg-pointer-invar \
	return-pointer-covar

all: $(EXE_TARGETS)
.PHONY: all

return-pointer-covar: return-pointer-covar.cpp
	$(CXX) $(LDFLAGS) -o $@ $^ $(CXXFLAGS)

arg-pointer-invar: arg-pointer-invar.cpp
	$(CXX) $(LDFLAGS) -o $@ $^ $(CXXFLAGS)

clean:
	rm -f a.out *.o *.a $(EXE_TARGETS)

format:
	clang-format -style=file -i *.cpp

.PHONY: clean format
