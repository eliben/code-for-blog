CXX = g++
CXXFLAGS = -O2 -std=c++11 -fno-inline -fno-exceptions

all: main sheeplib.so

sheeplib.so: sheeplib.cpp animal.h
	$(CXX) $(CXXFLAGS) -fpic -c $<
	$(CXX) -shared -o $@ sheeplib.o

main: main.cpp animal.h
	$(CXX) $(CXXFLAGS) $< -ldl -o $@

clean:
	rm -f *.o
	rm -f *.so
	rm -f main
