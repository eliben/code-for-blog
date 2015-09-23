CXX = g++
CXXFLAGS = -std=c++11 -Wall -O3 -g -DNDEBUG
LDFLAGS = -lpthread

# Point BENCH_PATH to where you have built the Google benchmark library
# (https://github.com/google/benchmark).
BENCH_PATH = $$HOME/test/benchmark
BENCH_INCLUDE = $(BENCH_PATH)/include
BENCH_LIBPATH = $(BENCH_PATH)/src
BENCH_ARCHIVE = $(BENCH_LIBPATH)/libbenchmark.a

all: rowcol_access

rowcol_access: rowcol_access.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@ \
		-I$(BENCH_INCLUDE) -L$(BENCH_LIBPATH) $(BENCH_ARCHIVE) $(LDFLAGS)

clean:
	rm -f rowcol_access
