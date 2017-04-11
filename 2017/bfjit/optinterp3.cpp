// A more optimized interpreter for BF.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <algorithm>
#include <fstream>
#include <iostream>
#include <stack>

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
  LOOP_SET_TO_ZERO,
  LOOP_MOVE_PTR,
  LOOP_MOVE_DATA,
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
  case BfOpKind::LOOP_SET_TO_ZERO:
    return "s";
  case BfOpKind::LOOP_MOVE_PTR:
    return "m";
  case BfOpKind::LOOP_MOVE_DATA:
    return "d";
  case BfOpKind::INVALID_OP:
    return "x";
  }
  return nullptr;
}

// Every op has a single numeric argument, which carries meaning specific to
// the op.
struct BfOp {
  BfOp(BfOpKind kind_param, int64_t argument_param)
      : kind(kind_param), argument(argument_param) {}

  // Serialize (emit textual representation for) this op onto the end of s.
  void serialize(std::string* s) const {
    *s += BfOpKind_name(kind) + std::to_string(argument);
  }

  BfOpKind kind = BfOpKind::INVALID_OP;
  int64_t argument = 0;
};

// Optimizes a loop that starts at loop_start (the opening JUMP_IF_DATA_ZERO).
// The loop runs until the end of ops (implicitly there's a back-jump after the
// last op in ops).
//
// If optimization succeeds, returns a sequence of instructions that replace the
// loop; otherwise, returns an empty vector.
std::vector<BfOp> optimize_loop(const std::vector<BfOp>& ops,
                                size_t loop_start) {
  std::vector<BfOp> new_ops;

  if (ops.size() - loop_start == 2) {
    BfOp repeated_op = ops[loop_start + 1];
    if (repeated_op.kind == BfOpKind::INC_DATA ||
        repeated_op.kind == BfOpKind::DEC_DATA) {
      new_ops.push_back(BfOp(BfOpKind::LOOP_SET_TO_ZERO, 0));
    } else if (repeated_op.kind == BfOpKind::INC_PTR ||
               repeated_op.kind == BfOpKind::DEC_PTR) {
      new_ops.push_back(
          BfOp(BfOpKind::LOOP_MOVE_PTR, repeated_op.kind == BfOpKind::INC_PTR
                                            ? repeated_op.argument
                                            : -repeated_op.argument));
    }
  } else if (ops.size() - loop_start == 5) {
    // Detect patterns: -<+> and ->+<
    if (ops[loop_start + 1].kind == BfOpKind::DEC_DATA &&
        ops[loop_start + 3].kind == BfOpKind::INC_DATA &&
        ops[loop_start + 1].argument == 1 &&
        ops[loop_start + 3].argument == 1) {
      if (ops[loop_start + 2].kind == BfOpKind::INC_PTR &&
          ops[loop_start + 4].kind == BfOpKind::DEC_PTR &&
          ops[loop_start + 2].argument == ops[loop_start + 4].argument) {
        new_ops.push_back(
            BfOp(BfOpKind::LOOP_MOVE_DATA, ops[loop_start + 2].argument));
      } else if (ops[loop_start + 2].kind == BfOpKind::DEC_PTR &&
                 ops[loop_start + 4].kind == BfOpKind::INC_PTR &&
                 ops[loop_start + 2].argument == ops[loop_start + 4].argument) {
        new_ops.push_back(
            BfOp(BfOpKind::LOOP_MOVE_DATA, -ops[loop_start + 2].argument));
      }
    }
  }
  return new_ops;
}

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

      // Try to optimize this loop; if optimize_loop succeeds, it returns a
      // non-empty vector which we can splice into ops in place of the loop.
      // If the returned vector is empty, we proceed as usual.
      std::vector<BfOp> optimized_loop =
          optimize_loop(ops, open_bracket_offset);

      if (optimized_loop.empty()) {
        // Loop wasn't optimized, so proceed emitting the back-jump to ops. We
        // have the offset of the matching '['. We can use it to create a new
        // jump op for the ']' we're handling, as well as patch up the offset of
        // the matching '['.
        ops[open_bracket_offset].argument = ops.size();
        ops.push_back(
            BfOp(BfOpKind::JUMP_IF_DATA_NOT_ZERO, open_bracket_offset));
      } else {
        // Replace this whole loop with optimized_loop.
        ops.erase(ops.begin() + open_bracket_offset, ops.end());
        ops.insert(ops.end(), optimized_loop.begin(), optimized_loop.end());
      }
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

void optinterp3(const Program& p, bool verbose) {
  // Initialize state.
  std::vector<uint8_t> memory(MEMORY_SIZE, 0);
  size_t dataptr = 0;

  Timer t1;
  const std::vector<BfOp> ops = translate_program(p);

  if (verbose) {
    std::cout << "* translation [elapsed " << t1.elapsed() << "s]:\n";

    for (size_t i = 0; i < ops.size(); ++i) {
      std::cout << " [" << i << "] " << BfOpKind_name(ops[i].kind) << " "
                << ops[i].argument << "\n";
    }
  }

  // Execute the translated ops in a for loop; pc always gets incremented by the
  // end of each iteration, though some ops may also move it in a less orderly
  // way.
  // Note: the pre-computation of ops_size shouldn't be necessary (since ops is
  // const) but it helps gcc 4.8 generate faster code.
  size_t ops_size = ops.size();
  for (size_t pc = 0; pc < ops_size; ++pc) {
    BfOp op = ops[pc];
    BfOpKind kind = op.kind;
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
      for (int i = 0; i < op.argument; ++i) {
        memory[dataptr] = std::cin.get();
      }
      break;
    case BfOpKind::WRITE_STDOUT:
      for (int i = 0; i < op.argument; ++i) {
        std::cout.put(memory[dataptr]);
      }
      break;
    case BfOpKind::LOOP_SET_TO_ZERO:
      memory[dataptr] = 0;
      break;
    case BfOpKind::LOOP_MOVE_PTR:
      while (memory[dataptr]) {
        dataptr += op.argument;
      }
      break;
    case BfOpKind::LOOP_MOVE_DATA: {
      if (memory[dataptr]) {
        int64_t move_to_ptr = static_cast<int64_t>(dataptr) + op.argument;
        memory[move_to_ptr] += memory[dataptr];
        memory[dataptr] = 0;
      }
      break;
    }
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
      break;
    }
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
    std::cout << "[>] Running optinterp3:\n";
  }

  Timer t2;
  optinterp3(program, verbose);

  if (verbose) {
    std::cout << "[<] Done (elapsed: " << t2.elapsed() << "s)\n";
  }

  return 0;
}
