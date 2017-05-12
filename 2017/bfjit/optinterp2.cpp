// An optimized interpreter for BF, take 2.
//
// In addition to the optimization of optinterp, here we squash repetitive
// sequences of all non-jump ops into single operations that carry the
// repetition number as an argument.
//
// Compile with -DBFTRACE to enable tracing in verbose mode.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <locale>
#include <stack>
#include <unordered_map>
#include <vector>

#include "parser.h"
#include "utils.h"

constexpr int MEMORY_SIZE = 30000;

enum class BfOpKind {
  INVALID_OP = 0,
  INC_PTR,
  DEC_PTR,
  INC_DATA,
  DEC_DATA,
  READ_STDIN,
  WRITE_STDOUT,
  JUMP_IF_DATA_ZERO,
  JUMP_IF_DATA_NOT_ZERO
};

const char* BfOpKind_name(BfOpKind kind) {
  switch (kind) {
  case BfOpKind::INC_PTR:
    return ">";
  case BfOpKind::DEC_PTR:
    return "<";
  case BfOpKind::INC_DATA:
    return "+";
  case BfOpKind::DEC_DATA:
    return "-";
  case BfOpKind::READ_STDIN:
    return ",";
  case BfOpKind::WRITE_STDOUT:
    return ".";
  case BfOpKind::JUMP_IF_DATA_ZERO:
    return "[";
  case BfOpKind::JUMP_IF_DATA_NOT_ZERO:
    return "]";
  case BfOpKind::INVALID_OP:
    return "x";
  }
  return nullptr;
}

// Every op has a single numeric argument. For JUMP_* ops it's the offset to
// which a jump should be made; for all other ops, it's the number of times the
// op is to be repeated.
struct BfOp {
  BfOp(BfOpKind kind_param, size_t argument_param)
      : kind(kind_param), argument(argument_param) {}

  // Serialize (emit textual representation for) this op onto the end of s.
  void serialize(std::string* s) const {
    *s += BfOpKind_name(kind) + std::to_string(argument);
  }

  BfOpKind kind = BfOpKind::INVALID_OP;
  size_t argument = 0;
};

// Translates the given program into a vector of BfOps that can be used for fast
// interpretation.
std::vector<BfOp> translate_program(const Program& p) {
  size_t pc = 0;
  size_t program_size = p.instructions.size();
  std::vector<BfOp> ops;

  // Throughout the translation loop, this stack contains offsets (in the ops
  // vector) of open brackets (JUMP_IF_DATA_ZERO ops) waiting for a closing
  // bracket. Since brackets nest, these naturally form a stack. The
  // JUMP_IF_DATA_ZERO ops get added to ops with their argument set to 0 until a
  // matching closing bracket is encountered, at which point the argument can be
  // back-patched.
  std::stack<size_t> open_bracket_stack;

  while (pc < program_size) {
    char instruction = p.instructions[pc];
    if (instruction == '[') {
      // Place a jump op with a placeholder 0 offset. It will be patched-up to
      // the right offset when the matching ']' is found.
      open_bracket_stack.push(ops.size());
      ops.push_back(BfOp(BfOpKind::JUMP_IF_DATA_ZERO, 0));
      pc++;
    } else if (instruction == ']') {
      if (open_bracket_stack.empty()) {
        DIE << "unmatched closing ']' at pc=" << pc;
      }
      size_t open_bracket_offset = open_bracket_stack.top();
      open_bracket_stack.pop();

      // Now we have the offset of the matching '['. We can use it to create a
      // new jump op for the ']' we're handling, as well as patch up the offset
      // of the matching '['.
      ops[open_bracket_offset].argument = ops.size();
      ops.push_back(BfOp(BfOpKind::JUMP_IF_DATA_NOT_ZERO, open_bracket_offset));
      pc++;
    } else {
      // Not a jump; all the other ops can be repeated, so find where the repeat
      // ends.
      size_t start = pc++;
      while (pc < program_size && p.instructions[pc] == instruction) {
        pc++;
      }
      // Here pc points to the first new instruction encountered, or to the end
      // of the program.
      size_t num_repeats = pc - start;

      // Figure out which op kind the instruction represents and add it to the
      // ops.
      BfOpKind kind = BfOpKind::INVALID_OP;
      switch (instruction) {
      case '>':
        kind = BfOpKind::INC_PTR;
        break;
      case '<':
        kind = BfOpKind::DEC_PTR;
        break;
      case '+':
        kind = BfOpKind::INC_DATA;
        break;
      case '-':
        kind = BfOpKind::DEC_DATA;
        break;
      case ',':
        kind = BfOpKind::READ_STDIN;
        break;
      case '.':
        kind = BfOpKind::WRITE_STDOUT;
        break;
      default: { DIE << "bad char '" << instruction << "' at pc=" << start; }
      }

      ops.push_back(BfOp(kind, num_repeats));
    }
  }

  return ops;
}

void optinterp2(const Program& p, bool verbose) {
  // Initialize state.
  std::vector<uint8_t> memory(MEMORY_SIZE, 0);
  size_t pc = 0;
  size_t dataptr = 0;

#ifdef BFTRACE
  std::unordered_map<int, size_t> op_exec_count;
  std::string current_trace;
  std::unordered_map<std::string, size_t> trace_count;
#endif

  Timer t1;
  std::vector<BfOp> ops = translate_program(p);

  if (verbose) {
    std::cout << "* translation [elapsed " << t1.elapsed() << "s]:\n";

    for (size_t i = 0; i < ops.size(); ++i) {
      std::cout << " [" << i << "] " << BfOpKind_name(ops[i].kind) << " "
                << ops[i].argument << "\n";
    }
  }

  // Execute the translated ops; pc points into ops, not into the program now.
  size_t ops_size = ops.size();
  while (pc < ops_size) {
    BfOp op = ops[pc];
    BfOpKind kind = op.kind;
#ifdef BFTRACE
    op_exec_count[static_cast<int>(kind)]++;
#endif
    switch (kind) {
    case BfOpKind::INC_PTR:
      dataptr += op.argument;
      break;
    case BfOpKind::DEC_PTR:
      dataptr -= op.argument;
      break;
    case BfOpKind::INC_DATA:
      memory[dataptr] += op.argument;
      break;
    case BfOpKind::DEC_DATA:
      memory[dataptr] -= op.argument;
      break;
    case BfOpKind::READ_STDIN:
      for (size_t i = 0; i < op.argument; ++i) {
        memory[dataptr] = std::cin.get();
      }
      break;
    case BfOpKind::WRITE_STDOUT:
      for (size_t i = 0; i < op.argument; ++i) {
        std::cout.put(memory[dataptr]);
      }
      break;
    case BfOpKind::JUMP_IF_DATA_ZERO:
      if (memory[dataptr] == 0) {
        pc = op.argument;
      }
      break;
    case BfOpKind::JUMP_IF_DATA_NOT_ZERO:
      if (memory[dataptr] != 0) {
        pc = op.argument;
      }
      break;
    case BfOpKind::INVALID_OP:
      DIE << "INVALID_OP encountered on pc=" << pc;
    }

#ifdef BFTRACE
    if (kind == BfOpKind::JUMP_IF_DATA_ZERO) {
      current_trace = "";
    } else if (kind == BfOpKind::JUMP_IF_DATA_NOT_ZERO) {
      if (current_trace.size() > 0) {
        trace_count[current_trace] += 1;
        current_trace = "";
      }
    } else {
      op.serialize(&current_trace);
    }
#endif

    pc++;
  }

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
      std::cout << BfOpKind_name(static_cast<BfOpKind>(i.first)) << "  -->  "
                << i.second << "\n";
      total += i.second;
    }
    std::cout << ".. Total: " << total << "\n\n";

    using TracePair = std::pair<std::string, size_t>;
    std::vector<TracePair> tracevec;
    std::copy(trace_count.begin(), trace_count.end(),
              std::back_inserter<std::vector<TracePair>>(tracevec));
    std::sort(tracevec.begin(), tracevec.end(),
              [](const TracePair& a, const TracePair& b) {
                return a.second > b.second;
              });

    for (auto const& t : tracevec) {
      std::cout << std::setw(15) << std::left << t.first << " --> " << t.second
                << "\n";
    }
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
    std::cout << "[>] Running optinterp2:\n";
  }

  Timer t2;
  optinterp2(program, verbose);

  if (verbose) {
    std::cout << "[<] Done (elapsed: " << t2.elapsed() << "s)\n";
  }

  return 0;
}
