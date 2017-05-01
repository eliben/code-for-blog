// Helper utilities for launching LLVM-based JIts.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include "llvm_jit_utils.h"
#include "utils.h"

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
// Dumping happens only in verbose mode (when verbose=true) in the constructor.
class ObjectDumpingCompiler {
public:
  ObjectDumpingCompiler(TargetMachine& target_machine, bool verbose)
      : target_machine_(target_machine), verbose_(verbose) {}

  object::OwningBinary<object::ObjectFile> operator()(Module& module) const {
    SmallVector<char, 0> obj_buffer_vec;
    raw_svector_ostream obj_stream(obj_buffer_vec);

    legacy::PassManager pass_manager;
    MCContext* context;
    if (target_machine_.addPassesToEmitMC(pass_manager, context, obj_stream)) {
      DIE << "Target does not support MC emission";
    }
    pass_manager.run(module);
    std::unique_ptr<MemoryBuffer> obj_buffer(
        new ObjectMemoryBuffer(std::move(obj_buffer_vec)));

    Expected<std::unique_ptr<object::ObjectFile>> obj =
        object::ObjectFile::createObjectFile(obj_buffer->getMemBufferRef());

    // Here we dump the emitted object to a raw binary file. This object is not
    // loaded (relocated) yet, so instructions like calls will have
    // placeholders.
    //
    // LLVM represents the object in memory as an ELF image. To dump the code,
    // we iterate to find the text section and emit its contents.
    if (verbose_) {
      bool found_text_section = false;
      for (auto& section : (*obj)->sections()) {
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
            const char* filename = "/tmp/llvmjit-out.bin";
            FILE* outfile = fopen(filename, "wb");
            if (outfile) {
              size_t n = sr.size();
              if (fwrite(sr.data(), 1, n, outfile) == n) {
                std::cout << "[*] emitted code to " << filename << "\n";
              }
              fclose(outfile);
            }
          }
        }
      }
    }

    typedef object::OwningBinary<object::ObjectFile> owning_obj;
    if (obj) {
      return owning_obj(std::move(*obj), std::move(obj_buffer));
    }
    consumeError(obj.takeError());
    return owning_obj(nullptr, nullptr);
  }

private:
  TargetMachine& target_machine_;
  bool verbose_;
};

} // namespace {

SimpleOrcJIT::SimpleOrcJIT(bool verbose)
    : verbose_(verbose), target_machine_(EngineBuilder().selectTarget()),
      data_layout_(target_machine_->createDataLayout()),
      compile_layer_(object_layer_,
                     ObjectDumpingCompiler(*target_machine_, verbose_)) {
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
  // This resolver looks back into the host with dlsym to find symbols the
  // module calls but aren't defined in it.
  auto resolver = orc::createLambdaResolver(
      [this](const std::string& name) {
        if (auto sym = find_mangled_symbol(name)) {
          return sym;
        }
        return JITSymbol(nullptr);
      },
      [](const std::string& name) {
        if (auto sym_addr =
                RTDyldMemoryManager::getSymbolAddressInProcess(name)) {
          return JITSymbol(sym_addr, JITSymbolFlags::Exported);
        }
        return JITSymbol(nullptr);
      });
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
  for (auto h : make_range(module_handles_.rbegin(), module_handles_.rend())) {
    if (auto sym =
            compile_layer_.findSymbolIn(h, name, exported_symbols_only)) {
      return sym;
    }
  }
  return nullptr;
}
