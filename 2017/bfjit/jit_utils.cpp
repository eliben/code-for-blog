// Utilities for writing a JIT.
//
// Note: the implementation is POSIX-specific, requiring mmap/munmap with
// appropriate flags.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include "jit_utils.h"
#include "utils.h"
#include <cassert>
#include <cstring>
#include <limits>
#include <sys/mman.h>

namespace {

// Allocates RW memory of given size and returns a pointer to it. On failure,
// prints out the error and returns nullptr. mmap is used to allocate, so
// deallocation has to be done with munmap, and the memory is allocated
// on a page boundary so it's suitable for calling mprotect.
void* alloc_writable_memory(size_t size) {
  void* ptr =
      mmap(0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (ptr == (void*)-1) {
    perror("mmap");
    return nullptr;
  }
  return ptr;
}

// Sets a RX permission on the given memory, which must be page-aligned. Returns
// 0 on success. On failure, prints out the error and returns -1.
int make_memory_executable(void* m, size_t size) {
  if (mprotect(m, size, PROT_READ | PROT_EXEC) == -1) {
    perror("mprotect");
    return -1;
  }
  return 0;
}

} // namespace

JitProgram::JitProgram(const std::vector<uint8_t>& code) {
  program_size_ = code.size();
  program_memory_ = alloc_writable_memory(program_size_);
  if (program_memory_ == nullptr) {
    DIE << "unable to allocate writable memory";
  }
  memcpy(program_memory_, code.data(), program_size_);
  if (make_memory_executable(program_memory_, program_size_) < 0) {
    DIE << "unable to mark memory as executable";
  }
}

JitProgram::~JitProgram() {
  if (program_memory_ != nullptr) {
    if (munmap(program_memory_, program_size_) < 0) {
      perror("munmap");
      DIE << "unable to unmap memory";
    }
  }
}

void CodeEmitter::EmitByte(uint8_t v) {
  code_.push_back(v);
}

void CodeEmitter::EmitBytes(std::initializer_list<uint8_t> seq) {
  for (auto v : seq) {
    EmitByte(v);
  }
}

void CodeEmitter::ReplaceByteAtOffset(size_t offset, uint8_t v) {
  assert(offset < code_.size() && "replacement fits in code");
  code_[offset] = v;
}

void CodeEmitter::ReplaceUint32AtOffset(size_t offset, uint32_t v) {
  ReplaceByteAtOffset(offset, v & 0xFF);
  ReplaceByteAtOffset(offset + 1, (v >> 8) & 0xFF);
  ReplaceByteAtOffset(offset + 2, (v >> 16) & 0xFF);
  ReplaceByteAtOffset(offset + 3, (v >> 24) & 0xFF);
}

void CodeEmitter::EmitUint32(uint32_t v) {
  EmitByte(v & 0xFF);
  EmitByte((v >> 8) & 0xFF);
  EmitByte((v >> 16) & 0xFF);
  EmitByte((v >> 24) & 0xFF);
}

void CodeEmitter::EmitUint64(uint64_t v) {
  EmitUint32(v & 0xFFFFFFFF);
  EmitUint32((v >> 32) & 0xFFFFFFFF);
}

uint32_t compute_relative_32bit_offset(size_t jump_from, size_t jump_to) {
  if (jump_to >= jump_from) {
    size_t diff = jump_to - jump_from;
    assert(diff < (1ull << 31));
    return diff;
  } else {
    // Here the diff is negative, so we need to encode it as 2s complement.
    size_t diff = jump_from - jump_to;
    assert(diff - 1 < (1ull << 31));
    uint32_t diff_unsigned = static_cast<uint32_t>(diff);
    return ~diff_unsigned + 1;
  }
}
