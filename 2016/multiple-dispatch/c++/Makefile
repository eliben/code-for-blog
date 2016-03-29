# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG
LDFLAGS =

EXECUTABLES = \
	doesntwork visitor bruteforce

all: $(EXECUTABLES)

bruteforce: bruteforce.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

doesntwork: doesntwork.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

visitor: visitor.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(EXECUTABLES)
