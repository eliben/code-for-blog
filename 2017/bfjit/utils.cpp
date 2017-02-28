// Some simple C++ utilities.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include "utils.h"
#include <cstdio>
#include <cstdlib>
#include <iostream>

namespace internal {

FatalLogMessage::~FatalLogMessage() {
  fprintf(stderr, "Fatal error: %s\n", str().c_str());
  exit(EXIT_FAILURE);
}

} // namespace internal

Timer::Timer() : t1_(std::chrono::high_resolution_clock::now()) {}

double Timer::elapsed() {
  auto t2 = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> elapsed = t2 - t1_;
  return elapsed.count();
}
