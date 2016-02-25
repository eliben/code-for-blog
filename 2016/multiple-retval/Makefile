# Makefile for building multiple-return-values samples on Linux
# 
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG -pthread
LDFLAGS = -lpthread -pthread

EXECUTABLES = \
	getline map-return-pair

all: $(EXECUTABLES)

map-return-pair: map-return-pair.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

getline: getline.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(EXECUTABLES)
