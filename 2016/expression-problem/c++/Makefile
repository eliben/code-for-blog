# Makefile for the patterns-composite-interpreter samples.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG
LDFLAGS =

EXECUTABLES = \
	basic-oop visitor

all: $(EXECUTABLES)

basic-oop: basic-oop.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

visitor: visitor.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

all: $(EXECUTABLES)

clean:
	rm -f $(EXECUTABLES)
