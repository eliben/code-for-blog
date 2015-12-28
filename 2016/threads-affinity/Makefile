# Makefile for building threading/affinity samples on Linux
# 
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG -pthread
LDFLAGS = -lpthread -pthread

# Some of the samples require the Google benchmark library to run their
# benchmarks [https://github.com/google/benchmark].
# Point BENCH_PATH to where you have the library installed/built.
BENCH_PATH = $$HOME/test/benchmark
BENCH_INCLUDE = $(BENCH_PATH)/include
BENCH_LIBPATH = $(BENCH_PATH)/src
BENCH_ARCHIVE = $(BENCH_LIBPATH)/libbenchmark.a

EXECUTABLES = launch-thread-per-cpu

all: $(EXECUTABLES)

launch-thread-per-cpu: launch-thread-per-cpu.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(EXECUTABLES)
