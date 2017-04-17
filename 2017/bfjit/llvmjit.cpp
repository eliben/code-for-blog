// TODO: switch to normal (non-LLVM) style

// TODO: mention how uninitialized dataptr leads LLVM to go crazy - cool example
// of undefined behavior "optimizations".
//

// TODO: Move JIT stuff to its own file, and in that file (in the implementation
// only) do using namespace llvm. In the other places, name it explicitly.
#include <fstream>
#include <iomanip>
#include <memory>
#include <stack>

#include "parser.h"
#include "utils.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/RuntimeDyld.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

// TODO: explain why keeping this on the stack makes sense for optimization.
constexpr int MEMORY_SIZE = 30000;
const char* const JIT_FUNC_NAME = "__llvmjit";

// TODO: this class should be internal.
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

// A type encapsulating simple Orc JIT functionality. Loosely based on the
// KaleidoscopeJIT example in the LLVM tree. Doesn't support cross-module
// symbol resolution; this JIT is best used with just a single module.
class SimpleOrcJIT {
public:
  // This sample doesn't implement on-request or lazy compilation. It therefore
  // uses Orc's eager compilation layer directly - IRCompileLayer. It also uses
  // the basis object layer - ObjectLinkingLayer - directly.
  using ObjLayerT = llvm::orc::ObjectLinkingLayer<>;
  using CompileLayerT = llvm::orc::IRCompileLayer<ObjLayerT>;
  using ModuleHandleT = CompileLayerT::ModuleSetHandleT;

  SimpleOrcJIT(bool verbose)
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

  const llvm::TargetMachine& get_target_machine() {
    return *target_machine_;
  }

  ModuleHandleT add_module(std::unique_ptr<llvm::Module> module) {
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
    auto handle = compile_layer_.addModuleSet(
        std::move(moduleset), make_unique<SectionMemoryManager>(),
        std::move(resolver));

    module_handles_.push_back(handle);
    return handle;
  }

  llvm::JITSymbol find_symbol(const std::string& name) {
    std::string mangled_name;
    {
      raw_string_ostream mangled_name_stream(mangled_name);
      Mangler::getNameWithPrefix(mangled_name_stream, name, data_layout_);
    }

    return find_mangled_symbol(mangled_name);
  }

  llvm::JITSymbol find_mangled_symbol(const std::string& name) {
    const bool exported_symbols_only = true;

    // Search modules in reverse order: from last added to first added.
    // This is the opposite of the usual search order for dlsym, but makes more
    // sense in a REPL where we want to bind to the newest available definition.
    for (auto h :
         make_range(module_handles_.rbegin(), module_handles_.rend())) {
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

private:
  bool verbose_;

  std::unique_ptr<llvm::TargetMachine> target_machine_;
  const llvm::DataLayout data_layout_;
  ObjLayerT object_layer_;
  CompileLayerT compile_layer_;
  std::vector<ModuleHandleT> module_handles_;
};

// Host function callable from JITed code. Given a pointer to program memory,
// dumps non-zero entries to cout.
extern "C" void dump_memory(uint8_t* memory) {
  std::cout << "* Memory nonzero locations:\n";
  for (size_t i = 0, pcount = 0; i < MEMORY_SIZE; ++i) {
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

struct BracketBlocks {
  BracketBlocks(BasicBlock* lbb, BasicBlock* plb)
      : loop_body_block(lbb), post_loop_block(plb) {}

  BasicBlock* loop_body_block;
  BasicBlock* post_loop_block;
};

Function* emit_jit_function(const Program& program, Module* module,
                            Function* dump_memory_func, Function* putchar_func,
                            Function* getchar_func) {
  LLVMContext& context = module->getContext();

  Type* int32_type = Type::getInt32Ty(context);
  Type* int8_type = Type::getInt8Ty(context);
  Type* void_type = Type::getVoidTy(context);

  FunctionType* jit_func_type = FunctionType::get(void_type, {}, false);
  Function* jit_func = Function::Create(
      jit_func_type, Function::ExternalLinkage, JIT_FUNC_NAME, module);

  BasicBlock* entry_bb = BasicBlock::Create(context, "entry", jit_func);
  IRBuilder<> builder(entry_bb);

  // Create stack allocations for the memory and the data pointer. The memory
  // is memset to zeros. The data pointer is used as an offset into the memory
  // array; it is initialized to 0.
  AllocaInst* memory =
      builder.CreateAlloca(int8_type, builder.getInt32(MEMORY_SIZE), "memory");
  builder.CreateMemSet(memory, builder.getInt8(0), MEMORY_SIZE, 1);
  AllocaInst* dataptr_addr =
      builder.CreateAlloca(int32_type, nullptr, "dataptr_addr");
  builder.CreateStore(builder.getInt32(0), dataptr_addr);

  std::stack<BracketBlocks> open_bracket_stack;

  for (size_t pc = 0; pc < program.instructions.size(); ++pc) {
    char instruction = program.instructions[pc];
    switch (instruction) {
    case '>': {
      Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      Value* inc_dataptr =
          builder.CreateAdd(dataptr, builder.getInt32(1), "inc_dataptr");
      builder.CreateStore(inc_dataptr, dataptr_addr);
      break;
    }
    case '<': {
      Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      Value* dec_dataptr =
          builder.CreateSub(dataptr, builder.getInt32(1), "dec_dataptr");
      builder.CreateStore(dec_dataptr, dataptr_addr);
      break;
    }
    case '+': {
      Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      Value* element_addr =
          builder.CreateInBoundsGEP(memory, {dataptr}, "element_addr");
      Value* element = builder.CreateLoad(element_addr, "element");
      Value* inc_element =
          builder.CreateAdd(element, builder.getInt8(1), "inc_element");
      builder.CreateStore(inc_element, element_addr);
      break;
    }
    case '-': {
      Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      Value* element_addr =
          builder.CreateInBoundsGEP(memory, {dataptr}, "element_addr");
      Value* element = builder.CreateLoad(element_addr, "element");
      Value* dec_element =
          builder.CreateSub(element, builder.getInt8(1), "sub_element");
      builder.CreateStore(dec_element, element_addr);
      break;
    }
    case '.': {
      Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      Value* element_addr =
          builder.CreateInBoundsGEP(memory, {dataptr}, "element_addr");
      Value* element = builder.CreateLoad(element_addr, "element");
      Value* element_i32 =
          builder.CreateIntCast(element, int32_type, false, "element_i32_");
      builder.CreateCall(putchar_func, element_i32);
      break;
    }
    case ',': {
      Value* user_input = builder.CreateCall(getchar_func, {}, "user_input");
      Value* user_input_i8 =
          builder.CreateIntCast(user_input, int8_type, false, "user_input_i8_");
      Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      Value* element_addr =
          builder.CreateInBoundsGEP(memory, {dataptr}, "element_addr");
      builder.CreateStore(user_input_i8, element_addr);
      break;
    }
    case '[': {
      Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      Value* element_addr =
          builder.CreateInBoundsGEP(memory, {dataptr}, "element_addr");
      Value* element = builder.CreateLoad(element_addr, "element");
      Value* cmp =
          builder.CreateICmpEQ(element, builder.getInt8(0), "compare_zero");

      BasicBlock* loop_body_block =
          BasicBlock::Create(context, "loop_body", jit_func);
      BasicBlock* post_loop_block =
          BasicBlock::Create(context, "post_loop", jit_func);
      builder.CreateCondBr(cmp, post_loop_block, loop_body_block);
      open_bracket_stack.push(BracketBlocks(loop_body_block, post_loop_block));
      builder.SetInsertPoint(loop_body_block);
      break;
    }
    case ']': {
      if (open_bracket_stack.empty()) {
        DIE << "unmatched closing ']' at pc=" << pc;
      }
      BracketBlocks blocks = open_bracket_stack.top();
      open_bracket_stack.pop();

      Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      Value* element_addr =
          builder.CreateInBoundsGEP(memory, {dataptr}, "element_addr");
      Value* element = builder.CreateLoad(element_addr, "element");
      Value* cmp =
          builder.CreateICmpNE(element, builder.getInt8(0), "compare_zero");
      builder.CreateCondBr(cmp, blocks.loop_body_block, blocks.post_loop_block);
      builder.SetInsertPoint(blocks.post_loop_block);
      break;
    }
    default: { DIE << "bad char '" << instruction << "' at pc=" << pc; }
    }
  }

  builder.CreateCall(dump_memory_func, {memory});
  builder.CreateRetVoid();

  return jit_func;
}

void llvmjit(const Program& program, bool verbose) {
  LLVMContext context;
  std::unique_ptr<Module> module(new Module("bfmodule", context));

  // Add a declaration for external functions used in the JITed code.
  Type* int32_type = Type::getInt32Ty(context);
  Type* void_type = Type::getVoidTy(context);

  Function* putchar_func =
      Function::Create(FunctionType::get(int32_type, {int32_type}, false),
                       Function::ExternalLinkage, "putchar", module.get());
  Function* getchar_func =
      Function::Create(FunctionType::get(int32_type, {}, false),
                       Function::ExternalLinkage, "getchar", module.get());

  Function* dump_memory_func = Function::Create(
      FunctionType::get(void_type, {Type::getInt8PtrTy(context)}, false),
      Function::ExternalLinkage, "dump_memory", module.get());

  Function* jit_func = emit_jit_function(
      program, module.get(), dump_memory_func, putchar_func, getchar_func);

  std::cout << "---> Pre optimization module:\n";
  module->dump();

  if (verifyFunction(*jit_func, &errs())) {
    DIE << "Error verifying function... exiting";
  }

  // Now execute...

  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();

  SimpleOrcJIT jit(/*verbose=*/true);
  module->setDataLayout(jit.get_target_machine().createDataLayout());

  PassManagerBuilder pm_builder;
  pm_builder.OptLevel = 3;
  pm_builder.SizeLevel = 0;
  pm_builder.LoopVectorize = true;
  pm_builder.SLPVectorize = true;
  auto function_pm =
      llvm::make_unique<legacy::FunctionPassManager>(module.get());
  pm_builder.populateFunctionPassManager(*function_pm);
  function_pm->doInitialization();

  // TODO: looks like the optimizer just removes all data movement instructions;
  // also, the element increments aren't coalesced
  // I can try to dump IR and then run opt -O3 on it to see what passes do to
  // it.
  function_pm->run(*jit_func);
  std::cout << "---> Post optimization module:\n";
  module->dump();

  jit.add_module(std::move(module));

  JITSymbol jit_func_sym = jit.find_symbol(JIT_FUNC_NAME);
  if (!jit_func_sym) {
    DIE << "Unable to find symbol " << JIT_FUNC_NAME << " in module";
  }

  using JitFuncType = void (*)(void);
  JitFuncType jit_func_ptr =
      reinterpret_cast<JitFuncType>(jit_func_sym.getAddress());
  jit_func_ptr();
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
    std::cout << "Host CPU name: " << llvm::sys::getHostCPUName().str() << "\n";
    std::cout << "CPU features:\n";
    llvm::StringMap<bool> host_features;
    if (llvm::sys::getHostCPUFeatures(host_features)) {
      int linecount = 0;
      for (auto& feature : host_features) {
        if (feature.second) {
          std::cout << "  " << feature.first().str();
          if (++linecount % 4 == 0) {
            std::cout << "\n";
          }
        }
      }
    }
    std::cout << "\n";
  }

  if (verbose) {
    std::cout << "[>] Running llvmjit:\n";
  }

  Timer t2;
  llvmjit(program, verbose);

  if (verbose) {
    std::cout << "[<] Done (elapsed: " << t2.elapsed() << "s)\n";
  }

  return 0;
}
