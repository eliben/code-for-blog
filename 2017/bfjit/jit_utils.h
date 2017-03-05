// Utilities for writing a JIT.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#ifndef JIT_UTILS_H
#define JIT_UTILS_H

#include <cstddef>
#include <cstdint>
#include <vector>

// Represents a JITed program in memory. Create it with a vector of code
// encoded as a binary sequence.
//
// The constructor maps memory with proper permissions and copies the
// code into it. The pointer returned by program_memory() then points to
// the code in executable memory. When JitProgram dies, it automatically
// cleans up the memory it mapped.
class JitProgram {
public:
  JitProgram(const std::vector<uint8_t>& code);
  ~JitProgram();

  // Get the pointer to program memory. This pointer is valid only as long as
  // the JitProgram object is alive.
  void* program_memory() {
    return program_memory_;
  }

  size_t program_size() {
    return program_size_;
  }

private:
  void* program_memory_ = nullptr;
  size_t program_size_ = 0;
};

// Helps emit a binary stream of code into a buffer. Entities larger than 8 bits
// are emitted in little endian.
class CodeEmitter {
public:
  CodeEmitter() = default;

  void EmitByte(uint8_t v);

  // Emits a sequence of consecutive bytes.
  void EmitBytes(std::initializer_list<uint8_t> seq);

  void EmitUint32(uint32_t v);
  void EmitUint64(uint64_t v);

  // Replaces the byte at 'offset' with 'v'. Assumes offset < size().
  void ReplaceByteAtOffset(size_t offset, uint8_t v);

  // Replaces the 32-bit word at 'offset' with 'v'. Assumes offset + 3 < size().
  void ReplaceUint32AtOffset(size_t offset, uint32_t v);

  size_t size() const {
    return code_.size();
  }

  const std::vector<uint8_t>& code() const {
    return code_;
  }

private:
  std::vector<uint8_t> code_;
};

// Computes a 32-bit relative offset for pc-relative jumps. Given an address to
// jump *from* and an address to jump *to*, returns the 4-byte integer that has
// to be encoded into the offset field of a jmp instruction. The user is
// expected to adjust jump addresses before passing them to this function (for
// example taking into account that a jump offset is computed from after the
// jump instruction itself).
// Note: this is very specific to the x64 architecture.
uint32_t compute_relative_32bit_offset(size_t jump_from, size_t jump_to);

#endif /* JIT_UTILS_H */
