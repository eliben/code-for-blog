# Makefile for building threading/affinity samples on Linux
# 
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG -pthread
LDFLAGS = -lpthread -pthread

EXECUTABLES = \
	launch-thread-per-cpu launch-threads-report-cpu thread-id-native-handle \
	set-affinity thread-workload-benchmark

all: $(EXECUTABLES)

thread-id-native-handle: thread-id-native-handle.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

launch-thread-per-cpu: launch-thread-per-cpu.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

launch-threads-report-cpu: launch-threads-report-cpu.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

set-affinity: set-affinity.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

thread-workload-benchmark: thread-workload-benchmark.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(EXECUTABLES)
