#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG
LDFLAGS =

EXECUTABLES = \
	problem-statement

all: $(EXECUTABLES)

problem-statement: problem-statement.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(EXECUTABLES)

