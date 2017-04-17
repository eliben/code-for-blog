// Helper utilities for launching LLVM-based JIts.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include "llvm_jit_utils.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/DynamicLibrary.h"

using namespace llvm;

namespace {

// ObjectDumpingCompiler is a copycat of Orc JIT's SimpleCompiler, with added
// dumping of the generated object file so we can inspect the final machine code
// produced by LLVM. Note that no IR-level optimizations are performed here.
class ObjectDumpingCompiler {
public:
  ObjectDumpingCompiler(TargetMachine& TM) : TM(TM) {}

  object::OwningBinary<object::ObjectFile> operator()(Module& M) const {
    SmallVector<char, 0> ObjBufferSV;
    raw_svector_ostream ObjStream(ObjBufferSV);

    legacy::PassManager PM;
    MCContext* Ctx;
    if (TM.addPassesToEmitMC(PM, Ctx, ObjStream)) {
      DIE << "Target does not support MC emission";
    }
    PM.run(M);
    std::unique_ptr<MemoryBuffer> ObjBuffer(
        new ObjectMemoryBuffer(std::move(ObjBufferSV)));

    Expected<std::unique_ptr<object::ObjectFile>> Obj =
        object::ObjectFile::createObjectFile(ObjBuffer->getMemBufferRef());

    bool found_text_section = false;
    for (auto& section : (*Obj)->sections()) {
      if (section.isText()) {
        if (found_text_section) {
          StringRef section_name;
          section.getName(section_name);
          DIE << "Found text section already; also found "
              << section_name.str();
        }
        found_text_section = true;

        StringRef sr;
        auto err = section.getContents(sr);
        if (!err) {
          std::cout << "** got section contents\n";
          std::string filename = "/tmp/jitout.bin";
          FILE* outfile = fopen(filename.c_str(), "wb");
          if (outfile) {
            size_t n = sr.size();
            if (fwrite(sr.data(), 1, n, outfile) == n) {
              std::cout << "** emitted code to " << filename << "\n";
            }
            fclose(outfile);
          }
        }
      }
    }

    typedef object::OwningBinary<object::ObjectFile> OwningObj;
    if (Obj)
      return OwningObj(std::move(*Obj), std::move(ObjBuffer));
    consumeError(Obj.takeError());
    return OwningObj(nullptr, nullptr);
  }

private:
  TargetMachine& TM;
};

} // namespace {

SimpleOrcJIT::SimpleOrcJIT(bool verbose)
    : verbose_(verbose), target_machine_(EngineBuilder().selectTarget()),
      data_layout_(target_machine_->createDataLayout()),
      compile_layer_(object_layer_, ObjectDumpingCompiler(*target_machine_)) {
  std::string error_string;
  if (llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr,
                                                        &error_string)) {
    DIE << "Failed to LoadLibraryPermanently: " << error_string;
  }

  if (verbose_) {
    Triple triple = target_machine_->getTargetTriple();
    std::cout << "JIT target machine:\n";
    std::cout << "  triple: " << triple.str() << "\n";
    std::cout << "  target cpu: " << target_machine_->getTargetCPU().str()
              << "\n";
    std::cout << "  target features: "
              << target_machine_->getTargetFeatureString().str() << "\n";
  }
}

void SimpleOrcJIT::add_module(std::unique_ptr<llvm::Module> module) {
  // We need a memory manager to allocate memory and resolve symbols for this
  // new module. Create one that resolves symbols by looking back into the
  // JIT.
  auto resolver = orc::createLambdaResolver(
      [&](const std::string& name) {
        if (auto sym = find_mangled_symbol(name)) {
          return sym;
        }
        return JITSymbol(nullptr);
      },
      [](const std::string&) { return nullptr; });
  std::vector<std::unique_ptr<llvm::Module>> moduleset;
  moduleset.push_back(std::move(module));
  auto handle = compile_layer_.addModuleSet(std::move(moduleset),
                                            make_unique<SectionMemoryManager>(),
                                            std::move(resolver));

  module_handles_.push_back(handle);
}

llvm::JITSymbol SimpleOrcJIT::find_symbol(const std::string& name) {
  std::string mangled_name;
  {
    raw_string_ostream mangled_name_stream(mangled_name);
    Mangler::getNameWithPrefix(mangled_name_stream, name, data_layout_);
  }

  return find_mangled_symbol(mangled_name);
}

llvm::JITSymbol SimpleOrcJIT::find_mangled_symbol(const std::string& name) {
  const bool exported_symbols_only = true;

  // Search modules in reverse order: from last added to first added.
  // This is the opposite of the usual search order for dlsym, but makes more
  // sense in a REPL where we want to bind to the newest available definition.
  for (auto h : make_range(module_handles_.rbegin(), module_handles_.rend())) {
    if (auto sym =
            compile_layer_.findSymbolIn(h, name, exported_symbols_only)) {
      return sym;
    }
  }

  // If we can't find the symbol in the JIT, try looking in the host process.
  if (auto sym_addr = RTDyldMemoryManager::getSymbolAddressInProcess(name)) {
    return JITSymbol(sym_addr, JITSymbolFlags::Exported);
  }

  return nullptr;
}
