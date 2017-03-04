// An optimized interpreter for BF, take 1.
//
// Instead of finding the jump destinations by walking the program at runtime,
// precomputes them in a "jump table" before the program is run.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <memory>
#include <sstream>
#include <unordered_map>
#include <vector>

#include "parser.h"
#include "utils.h"

constexpr int MEMORY_SIZE = 30000;

// The jump table is a vector of the same length as the program. For each '[' or
// ']' instruction in the program located at offset i, jumptable[i] will be the
// offset of the matching bracket; this can be used to efficiently find the
// matching bracket at runtime. For all other instructions, jumptable[i] is 0.
std::vector<size_t> compute_jumptable(const Program& p) {
  size_t pc = 0;
  size_t program_size = p.instructions.size();
  std::vector<size_t> jumptable(program_size, 0);

  while (pc < program_size) {
    char instruction = p.instructions[pc];
    if (instruction == '[') {
      int bracket_nesting = 1;
      size_t seek = pc;

      while (bracket_nesting && ++seek < program_size) {
        if (p.instructions[seek] == ']') {
          bracket_nesting--;
        } else if (p.instructions[seek] == '[') {
          bracket_nesting++;
        }
      }

      if (!bracket_nesting) {
        jumptable[pc] = seek;
        jumptable[seek] = pc;
      } else {
        DIE << "unmatched '[' at pc=" << pc;
      }
    }
    pc++;
  }

  return jumptable;
}

void optinterp(const Program& p, bool verbose) {
  // Initialize state.
  std::vector<uint8_t> memory(MEMORY_SIZE, 0);
  size_t pc = 0;
  size_t dataptr = 0;

  Timer t1;
  std::vector<size_t> jumptable = compute_jumptable(p);

#ifdef BFTRACE
  std::unordered_map<char, size_t> op_exec_count;
#endif

  if (verbose) {
    std::cout << "* jumptable [elapsed " << t1.elapsed() << "s]: ";
    for (size_t i = 0; i < jumptable.size(); ++i) {
      if (jumptable[i]) {
        std::cout << "[" << i << "]=" << jumptable[i] << " ";
      }
    }
    std::cout << "\n";
  }

  // Pre-caching the program size shouldn't make a difference since p is const&,
  // but it does with gcc 4.8.4, speeding the interpreter up by 10%. It doesn't
  // affect clang 3.7, however.
  size_t program_size = p.instructions.size();
  while (pc < program_size) {
    char instruction = p.instructions[pc];
#ifdef BFTRACE
    op_exec_count[instruction]++;
#endif
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
        pc = jumptable[pc];
      }
      break;
    case ']':
      if (memory[dataptr] != 0) {
        pc = jumptable[pc];
      }
      break;
    default: { DIE << "bad char '" << instruction << "' at pc=" << pc; }
    }

    // The jumptable points to the matching bracket; in all cases, pc needs to
    // be advanced by one (jump or no jump).
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

#ifdef BFTRACE
    std::cout << "* Tracing:\n";
    std::cout.imbue(std::locale(""));
    size_t total = 0;
    for (auto i : op_exec_count) {
      std::cout << i.first << "  -->  " << i.second << "\n";
      total += i.second;
    }
    std::cout << ".. Total: " << total << "\n";
#endif
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
    std::cout << "[>] Running optinterp:\n";
  }

  Timer t2;
  optinterp(program, verbose);

  if (verbose) {
    std::cout << "[<] Done (elapsed: " << t2.elapsed() << "s)\n";
  }

  return 0;
}
