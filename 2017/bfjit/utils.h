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

// Parses the command-line for BF executors, to obtain the bf file path and
// values for flags. These are taken by pointers and assigned in this function.
// If any error occurs during parsing, this function reports it and exits.
// All flags are expected to be supplied before the positional bf file path,
// which has to be last on the command line.
void parse_command_line(int argc, const char** argv, std::string* bf_file_path,
                        bool* verbose);

#endif /* UTILS_H */
