# Makefile for expression-problem samples.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG
LDFLAGS =

EXECUTABLES = \
	basic-oop visitor-dispatch-in-data visitor-dispatch-in-visitor \
	visitor-extended

all: $(EXECUTABLES)

basic-oop: basic-oop.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

visitor-dispatch-in-data: visitor-dispatch-in-data.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

visitor-dispatch-in-visitor: visitor-dispatch-in-visitor.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

visitor-extended: visitor-extended.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

all: $(EXECUTABLES)

clean:
	rm -f $(EXECUTABLES)
