# Makefile for the patterns-composite-interpreter samples.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG
LDFLAGS =

EXECUTABLES = \
<<<<<<< HEAD:2016/expression-problem/c++/Makefile
	basic-oop visitor

all: $(EXECUTABLES)

basic-oop: basic-oop.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

visitor: visitor.cpp
=======
	interp

all: $(EXECUTABLES)

problem-statement: interp.cpp
>>>>>>> 1caa726d06ddc1609c34d2fc530580e3d5ba95a0:2016/patterns-composite-interpreter/Makefile
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(EXECUTABLES)
