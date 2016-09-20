# Makefile for readline samples.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CC = gcc
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG
CCFLAGS =
LDFLAGS = -lreadline

EXECUTABLES = \
	readline-keep-history fileman-example readline-complete-simple \
	readline-complete-hierarchical readline-complete-nogen

all: $(EXECUTABLES)

readline-keep-history: readline-keep-history.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

readline-complete-simple: readline-complete-simple.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

readline-complete-hierarchical: readline-complete-hierarchical.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

readline-complete-nogen: readline-complete-nogen.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

fileman-example: fileman-example.c
	$(CC) $(CCFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f core $(EXECUTABLES)
