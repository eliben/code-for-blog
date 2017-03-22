// A simple (unoptimized) interpreter for BF.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <memory>
#include <sstream>
#include <vector>

#include "parser.h"
#include "utils.h"

constexpr int MEMORY_SIZE = 30000;

void simpleinterp(const Program& p, bool verbose) {
  // Initialize state.
  std::vector<uint8_t> memory(MEMORY_SIZE, 0);
  size_t pc = 0;
  size_t dataptr = 0;

  while (pc < p.instructions.size()) {
    char instruction = p.instructions[pc];
    switch (instruction) {
    case '>':
      dataptr++;
      break;
    case '<':
      dataptr--;
      break;
    case '+':
      memory[dataptr]++;
      break;
    case '-':
      memory[dataptr]--;
      break;
    case '.':
      std::cout.put(memory[dataptr]);
      break;
    case ',':
      memory[dataptr] = std::cin.get();
      break;
    case '[':
      if (memory[dataptr] == 0) {
        int bracket_nesting = 1;
        size_t saved_pc = pc;

        while (bracket_nesting && ++pc < p.instructions.size()) {
          if (p.instructions[pc] == ']') {
            bracket_nesting--;
          } else if (p.instructions[pc] == '[') {
            bracket_nesting++;
          }
        }

        if (!bracket_nesting) {
          break;
        } else {
          DIE << "unmatched '[' at pc=" << saved_pc;
        }
      }
      break;
    case ']':
      if (memory[dataptr] != 0) {
        int bracket_nesting = 1;
        size_t saved_pc = pc;

        while (bracket_nesting && pc > 0) {
          pc--;
          if (p.instructions[pc] == '[') {
            bracket_nesting--;
          } else if (p.instructions[pc] == ']') {
            bracket_nesting++;
          }
        }

        if (!bracket_nesting) {
          break;
        } else {
          DIE << "unmatched ']' at pc=" << saved_pc;
        }
      }
      break;
    default: { DIE << "bad char '" << instruction << "' at pc=" << pc; }
    }

    pc++;
  }

  // Done running the program. Dump state if verbose.
  if (verbose) {
    std::cout << "* pc=" << pc << "\n";
    std::cout << "* dataptr=" << dataptr << "\n";
    std::cout << "* Memory nonzero locations:\n";

    for (size_t i = 0, pcount = 0; i < memory.size(); ++i) {
      if (memory[i]) {
        std::cout << std::right << "[" << std::setw(3) << i
                  << "] = " << std::setw(3) << std::left
                  << static_cast<int32_t>(memory[i]) << "      ";
        pcount++;

        if (pcount > 0 && pcount % 4 == 0) {
          std::cout << "\n";
        }
      }
    }
    std::cout << "\n";
  }
}

int main(int argc, const char** argv) {
  bool verbose = false;
  std::string bf_file_path;
  parse_command_line(argc, argv, &bf_file_path, &verbose);

  Timer t1;
  std::ifstream file(bf_file_path);
  if (!file) {
    DIE << "unable to open file " << bf_file_path;
  }
  Program program = parse_from_stream(file);

  if (verbose) {
    std::cout << "Parsing took: " << t1.elapsed() << "s\n";
    std::cout << "Length of program: " << program.instructions.size() << "\n";
    std::cout << "Program:\n" << program.instructions << "\n";
  }

  if (verbose) {
    std::cout << "[>] Running simpleinterp:\n";
  }

  Timer t2;
  simpleinterp(program, verbose);

  if (verbose) {
    std::cout << "[<] Done (elapsed: " << t2.elapsed() << "s)\n";
  }

  return 0;
}
