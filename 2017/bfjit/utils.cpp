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

namespace {

void usage_and_exit(const std::string& progname) {
  std::cout << "Expecting " << progname << " [flags] <BF file>\n";
  std::cout << "\nSupported flags:\n";
  std::cout << "    --verbose           enable verbose output\n";
  exit(EXIT_SUCCESS);
}

} // namespace {

void parse_command_line(int argc, const char** argv, std::string* bf_file_path,
                        bool* verbose) {
  *verbose = false;

  // This loop handles flags that optionally come before the actual arguments.
  // When it's done, arg_i will point to the first non-flag argument.
  int arg_i = 1;
  for (; arg_i < argc; ++arg_i) {
    std::string arg = argv[arg_i];
    if (!(arg.size() > 2 && arg[0] == '-' && arg[1] == '-')) {
      // If this arg doesn't start with a --, it's not a flag. So we expect it
      // to be the BF program.
      break;
    } else if (arg == "--verbose") {
      *verbose = true;
    } else if (arg == "--help") {
      usage_and_exit(argv[0]);
    } else {
      usage_and_exit(argv[0]);
    }
  }

  if (arg_i >= argc) {
    usage_and_exit(argv[0]);
  }
  *bf_file_path = argv[arg_i];
}
