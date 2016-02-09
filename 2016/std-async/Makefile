# Makefile for building threading/async samples on Linux
# 
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG -pthread
LDFLAGS = -lpthread -pthread

EXECUTABLES = \
	accum-thread-vs-async using-std-future thread-exception async-thread-local \
	async-mutex-unlock

all: $(EXECUTABLES)

accum-thread-vs-async: accum-thread-vs-async.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

using-std-future: using-std-future.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

thread-exception: thread-exception.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

async-mutex-unlock: async-mutex-unlock.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

async-thread-local: async-thread-local.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(EXECUTABLES)
