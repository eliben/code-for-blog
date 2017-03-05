// A simple JIT for BF, without spending much on optimization.
//
// Uses no external libraries - emits the JITed native code directly.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#ifndef SIMPLE_JIT_H
#define SIMPLE_JIT_H

#include <cstdint>
#include <iostream>
#include <vector>
#include "executor.h"
#include "parser.h"

class SimpleJit : public Executor {
public:
  void run(const Program& p, bool verbose) override;
  void dump_state(std::ostream& stream) override;

  const char* name() override {
    return "SimpleJit";
  }

private:
  std::vector<uint8_t> emitted_code_;
  std::vector<uint8_t> memory_;
};

#endif /* SIMPLE_JIT_H */
