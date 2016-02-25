# Makefile for building RAII samples on Linux
#
# -fno-exceptions disables exceptions
# 
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG -pthread -fno-exceptions
LDFLAGS = -lpthread -pthread

EXECUTABLES = \
	file-raii refcount-raii uniqueptr-raii

all: $(EXECUTABLES)

file-raii: file-raii.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

refcount-raii: refcount-raii.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

uniqueptr-raii: uniqueptr-raii.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(EXECUTABLES)
