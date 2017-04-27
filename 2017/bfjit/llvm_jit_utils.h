// Helper utilities for launching LLVM-based JIts.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#ifndef LLVM_JIT_UTILS_H
#define LLVM_JIT_UTILS_H

#include <iostream>
#include <vector>

#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/RuntimeDyld.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"

// A type encapsulating simple Orc JIT functionality. Loosely based on the
// KaleidoscopeJIT example in the LLVM tree. Doesn't support cross-module
// symbol resolution; this JIT is best used with just a single module.
class SimpleOrcJIT {
public:
  // Initialize the JIT. In verbose mode extra information will be dumped. The
  // JIT is created with a default target machine.
  SimpleOrcJIT(bool verbose);

  // Get access to the target machine used by the JIT.
  const llvm::TargetMachine& get_target_machine() {
    return *target_machine_;
  }

  // Add an LLVM module to the JIT. The JIT takes ownership.
  void add_module(std::unique_ptr<llvm::Module> module);

  // Find a symbol in JITed code. name is plain, unmangled. SimpleOrcJIT will
  // mangle it internally.
  llvm::JITSymbol find_symbol(const std::string& name);

private:
  // This sample doesn't implement on-request or lazy compilation. It therefore
  // uses Orc's eager compilation layer directly - IRCompileLayer. It also uses
  // the basis object layer - ObjectLinkingLayer - directly.
  using ObjLayerT = llvm::orc::ObjectLinkingLayer<>;
  using CompileLayerT = llvm::orc::IRCompileLayer<ObjLayerT>;
  using ModuleHandleT = CompileLayerT::ModuleSetHandleT;

  // Helper method to look for symbols that already have mangled names.
  llvm::JITSymbol find_mangled_symbol(const std::string& name);

  bool verbose_;

  std::unique_ptr<llvm::TargetMachine> target_machine_;
  const llvm::DataLayout data_layout_;
  ObjLayerT object_layer_;
  CompileLayerT compile_layer_;
  std::vector<ModuleHandleT> module_handles_;
};

#endif /* LLVM_JIT_UTILS_H */
