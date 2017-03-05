// Unit tests for jit_utils.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include "jit_utils.h"

#include <cassert>

#ifdef NDEBUG
#error Cannot compile test with asserts disabled
#endif // NDEBUG

#include <iomanip>
#include <iostream>

void test_compute_relative_32bit_offset() {
  // Basic cases with offset +10 or -10
  assert(compute_relative_32bit_offset(20, 30) == 10);
  assert(compute_relative_32bit_offset(40, 30) == 0xFFFFFFF6);

  // Offset 0, for sanity
  assert(compute_relative_32bit_offset(20, 20) == 0);

  // Offsets 1 and -1, corner cases
  assert(compute_relative_32bit_offset(100, 101) == 1);
  assert(compute_relative_32bit_offset(101, 100) == 0xFFFFFFFF);

  // Larger offsets, +/- 256
  assert(compute_relative_32bit_offset(1000, 1256) == 256);
  assert(compute_relative_32bit_offset(1256, 1000) == 0xFFFFFF00);

  // From/to addresses live in high 32-bits of 64-bit space, with a diff of 10
  assert(compute_relative_32bit_offset(0x20000000CCCCCC00ull,
                                       0x20000000CCCCCC0Aull) == 10);
  assert(compute_relative_32bit_offset(0x20000000CCCCCC0Aull,
                                       0x20000000CCCCCC00ull) == 0xFFFFFFF6);

  // Jumps over the 32-bit boundary
  assert(compute_relative_32bit_offset(0xFFFFFFFF, 0x10000000C) == 13);
  assert(compute_relative_32bit_offset(0x10000000C, 0xFFFFFFFF) == 0xFFFFFFF3);

  // Jumps over the 31-bit boundary
  assert(compute_relative_32bit_offset(0x7FFFFFFF, 0x80000001) == 2);
  assert(compute_relative_32bit_offset(0x80000001, 0x7FFFFFFF) == 0xFFFFFFFE);

  // Maximal diffs supported
  assert(compute_relative_32bit_offset(0x2020202000000000,
                                       0x202020207FFFFFFF) == 0x7FFFFFFF);
  assert(compute_relative_32bit_offset(0x2020202080000000,
                                       0x2020202000000000) == 0x80000000);
}

void test_code_emitter() {
  // Test basic emission methods
  CodeEmitter em1;
  em1.EmitByte(0x20);
  assert(em1.size() == 1);
  assert(em1.code()[0] == 0x20);

  em1.EmitByte(0x30);
  assert(em1.size() == 2);
  assert(em1.code()[0] == 0x20);
  assert(em1.code()[1] == 0x30);

  em1.EmitUint32(0xA0B0C0D0);
  assert(em1.size() == 6);
  assert(em1.code()[2] == 0xD0);
  assert(em1.code()[3] == 0xC0);
  assert(em1.code()[4] == 0xB0);
  assert(em1.code()[5] == 0xA0);

  em1.EmitUint64(0x1112131415161718ull);
  assert(em1.size() == 14);
  assert(em1.code()[6] == 0x18);
  assert(em1.code()[13] == 0x11);

  CodeEmitter em2;
  em2.EmitBytes({0x01, 0x03, 0x05});
  assert(em2.size() == 3);
  assert(em2.code()[0] == 0x01);
  assert(em2.code()[1] == 0x03);
  assert(em2.code()[2] == 0x05);

  // Now test the replacement methods
  CodeEmitter em3;
  em3.EmitBytes({0x01, 0x02, 0x03, 0x04, 0x11, 0x12, 0x13, 0x14, 0x21, 0x22,
                 0x23, 0x24, 0x31, 0x32, 0x33, 0x34});
  assert(em3.size() == 16);
  assert(em3.code()[10] == 0x23);
  assert(em3.code()[15] == 0x34);

  em3.ReplaceByteAtOffset(10, 0x55);
  assert(em3.size() == 16);
  assert(em3.code()[10] == 0x55);

  em3.ReplaceUint32AtOffset(12, 0xF2E2D2C2);
  assert(em3.size() == 16);
  assert(em3.code()[15] == 0xF2);
}

void test_jit_program() {
  // Tests that JitProgram works for emitting and running a simple function.
  std::vector<uint8_t> code{
      0x48, 0x89, 0xf8,       // mov %rdi, %rax
      0x48, 0x83, 0xc0, 0x04, // add $4, %rax
      0xc3                    // ret
  };

  JitProgram jit_program(code);

  using JittedFunc = long (*)(long);
  JittedFunc func = (JittedFunc)jit_program.program_memory();
  assert(func(21) == 25);
}

void test_all() {
  test_compute_relative_32bit_offset();
  test_code_emitter();
  test_jit_program();
}

int main(int argc, const char** argv) {
  std::cout << "jit_utils_test.....";
  test_all();
  std::cout << "OK\n";
  return 0;
}
