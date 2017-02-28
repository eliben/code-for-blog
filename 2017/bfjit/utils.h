// Some simple C++ utilities.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#ifndef UTILS_H
#define UTILS_H

#include <chrono>
#include <sstream>
#include <string>

namespace internal {

// Helper class to implement DIE.
class FatalLogMessage : public std::basic_ostringstream<char> {
public:
  ~FatalLogMessage();
};

} // namespace internal

// Can be used as a stream, and exits with an error after printing.
// For example:
//   DIE << "bad stuff " << foo << " " << bar;
#define DIE internal::FatalLogMessage()

// Simple Timer for high-precision timing measurements. Usage:
//
//   Timer t;
//   ... code to measure
//   t.elapsed() returns the number of seconds elapsed since t was created.
class Timer {
public:
  Timer();

  // Returns the number of seconds (as a high-precision floating point number)
  // elapsed since the construction of this Timer object.
  double elapsed();

private:
  std::chrono::time_point<std::chrono::high_resolution_clock> t1_;
};

#endif /* UTILS_H */
