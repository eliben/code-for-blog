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

# The following example requires HWLOC
# [https://www.open-mpi.org/projects/hwloc/] to be built and installed in
# HWLOC_INSTALL_PATH. It's not in EXECUTABLES so the default 'make' command
# skips it. Build explicitly if you have HWLOC installed.

HWLOC_INSTALL_PATH = $$HOME/test/hwloc/build

hwloc-example: hwloc-example.cpp
	$(CXX) $(CXXFLAGS) -I$(HWLOC_INSTALL_PATH)/include $^ -o $@ $(LDFLAGS) \
		-L$(HWLOC_INSTALL_PATH)/lib -lhwloc

clean:
	rm -f $(EXECUTABLES) hwloc-example
