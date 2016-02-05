# Makefile for building threading/async samples on Linux
# 
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG -pthread
LDFLAGS = -lpthread -pthread

EXECUTABLES = \
	accum-thread-vs-async

all: $(EXECUTABLES)

accum-thread-vs-async: accum-thread-vs-async.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(EXECUTABLES)
