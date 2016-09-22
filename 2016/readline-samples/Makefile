# Makefile for readline samples.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CC = gcc
CXXFLAGS = -std=c++11 -Wall -O3 -g
CCFLAGS =
LDFLAGS = -lreadline

EXECUTABLES = \
	noreadline readline-keep-history readline-complete-simple \
	readline-complete-hierarchical readline-complete-nogen \
  utils_test

all: $(EXECUTABLES)

noreadline: noreadline.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

readline-keep-history: readline-keep-history.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

readline-complete-simple: readline-complete-simple.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

readline-complete-hierarchical: readline-complete-hierarchical.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

readline-complete-nogen.o: readline-complete-nogen.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

readline-complete-nogen: readline-complete-nogen.o utils.o
	$(CXX) $^ -o $@ $(LDFLAGS)

utils.o: utils.cpp utils.h
	$(CXX) $(CXXFLAGS) -c $< -o $@

utils_test.o: utils_test.cpp utils.h
	$(CXX) $(CXXFLAGS) -c $< -o $@

utils_test: utils_test.o utils.o
	$(CXX) $^ -o $@

clean:
	rm -f core $(EXECUTABLES)
