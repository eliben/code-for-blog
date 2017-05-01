// An JIT for BF using LLVM. Compiles a BF program to LLVM IR, optimizes the IR
// using LLVM's backend and JIT-executes it.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <fstream>
#include <iomanip>
#include <memory>
#include <stack>

#include "llvm_jit_utils.h"
#include "parser.h"
#include "utils.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

constexpr int MEMORY_SIZE = 30000;
const char* const JIT_FUNC_NAME = "__llvmjit";

// Host function callable from JITed code. Given a pointer to program memory,
// dumps non-zero entries to std::cout. This function is declared extern "C" so
// that we can refer to it in the emitted LLVM IR without mangling the name.
// Alternatively, we could use the LLVM mangler to mangle the name for us (and
// then the extern "C" wouldn't be necessary).
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

// Helper function that prints the textual LLVM IR of module into a file.
void llvm_module_to_file(const llvm::Module& module, const char* filename) {
  std::string str;
  llvm::raw_string_ostream os(str);
  module.print(os, nullptr);

  std::ofstream of(filename);
  of << os.str();
}

// Keeps the state of LLVM basic blocks created for every matching bracket pair
// in BF ("[" ... "]").
struct BracketBlocks {
  BracketBlocks(llvm::BasicBlock* lbb, llvm::BasicBlock* plb)
      : loop_body_block(lbb), post_loop_block(plb) {}

  llvm::BasicBlock* loop_body_block;
  llvm::BasicBlock* post_loop_block;
};

llvm::Function* emit_jit_function(const Program& program, llvm::Module* module,
                                  llvm::Function* dump_memory_func,
                                  llvm::Function* putchar_func,
                                  llvm::Function* getchar_func, bool verbose) {
  llvm::LLVMContext& context = module->getContext();

  llvm::Type* int32_type = llvm::Type::getInt32Ty(context);
  llvm::Type* int8_type = llvm::Type::getInt8Ty(context);
  llvm::Type* void_type = llvm::Type::getVoidTy(context);

  llvm::FunctionType* jit_func_type =
      llvm::FunctionType::get(void_type, {}, false);
  llvm::Function* jit_func = llvm::Function::Create(
      jit_func_type, llvm::Function::ExternalLinkage, JIT_FUNC_NAME, module);

  llvm::BasicBlock* entry_bb =
      llvm::BasicBlock::Create(context, "entry", jit_func);
  llvm::IRBuilder<> builder(entry_bb);

  // Create stack allocations for the memory and the data pointer. The memory
  // is memset to zeros. The data pointer is used as an offset into the memory
  // array; it is initialized to 0.
  llvm::AllocaInst* memory =
      builder.CreateAlloca(int8_type, builder.getInt32(MEMORY_SIZE), "memory");
  builder.CreateMemSet(memory, builder.getInt8(0), MEMORY_SIZE, 1);
  llvm::AllocaInst* dataptr_addr =
      builder.CreateAlloca(int32_type, nullptr, "dataptr_addr");
  builder.CreateStore(builder.getInt32(0), dataptr_addr);

  std::stack<BracketBlocks> open_bracket_stack;

  for (size_t pc = 0; pc < program.instructions.size(); ++pc) {
    char instruction = program.instructions[pc];
    switch (instruction) {
    case '>': {
      llvm::Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      llvm::Value* inc_dataptr =
          builder.CreateAdd(dataptr, builder.getInt32(1), "inc_dataptr");
      builder.CreateStore(inc_dataptr, dataptr_addr);
      break;
    }
    case '<': {
      llvm::Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      llvm::Value* dec_dataptr =
          builder.CreateSub(dataptr, builder.getInt32(1), "dec_dataptr");
      builder.CreateStore(dec_dataptr, dataptr_addr);
      break;
    }
    case '+': {
      llvm::Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      llvm::Value* element_addr =
          builder.CreateInBoundsGEP(memory, {dataptr}, "element_addr");
      llvm::Value* element = builder.CreateLoad(element_addr, "element");
      llvm::Value* inc_element =
          builder.CreateAdd(element, builder.getInt8(1), "inc_element");
      builder.CreateStore(inc_element, element_addr);
      break;
    }
    case '-': {
      llvm::Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      llvm::Value* element_addr =
          builder.CreateInBoundsGEP(memory, {dataptr}, "element_addr");
      llvm::Value* element = builder.CreateLoad(element_addr, "element");
      llvm::Value* dec_element =
          builder.CreateSub(element, builder.getInt8(1), "sub_element");
      builder.CreateStore(dec_element, element_addr);
      break;
    }
    case '.': {
      llvm::Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      llvm::Value* element_addr =
          builder.CreateInBoundsGEP(memory, {dataptr}, "element_addr");
      llvm::Value* element = builder.CreateLoad(element_addr, "element");
      llvm::Value* element_i32 =
          builder.CreateIntCast(element, int32_type, false, "element_i32_");
      builder.CreateCall(putchar_func, element_i32);
      break;
    }
    case ',': {
      llvm::Value* user_input =
          builder.CreateCall(getchar_func, {}, "user_input");
      llvm::Value* user_input_i8 =
          builder.CreateIntCast(user_input, int8_type, false, "user_input_i8_");
      llvm::Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      llvm::Value* element_addr =
          builder.CreateInBoundsGEP(memory, {dataptr}, "element_addr");
      builder.CreateStore(user_input_i8, element_addr);
      break;
    }
    case '[': {
      llvm::Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      llvm::Value* element_addr =
          builder.CreateInBoundsGEP(memory, {dataptr}, "element_addr");
      llvm::Value* element = builder.CreateLoad(element_addr, "element");
      llvm::Value* cmp =
          builder.CreateICmpEQ(element, builder.getInt8(0), "compare_zero");

      llvm::BasicBlock* loop_body_block =
          llvm::BasicBlock::Create(context, "loop_body", jit_func);
      llvm::BasicBlock* post_loop_block =
          llvm::BasicBlock::Create(context, "post_loop", jit_func);
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

      llvm::Value* dataptr = builder.CreateLoad(dataptr_addr, "dataptr");
      llvm::Value* element_addr =
          builder.CreateInBoundsGEP(memory, {dataptr}, "element_addr");
      llvm::Value* element = builder.CreateLoad(element_addr, "element");
      llvm::Value* cmp =
          builder.CreateICmpNE(element, builder.getInt8(0), "compare_zero");
      builder.CreateCondBr(cmp, blocks.loop_body_block, blocks.post_loop_block);
      builder.SetInsertPoint(blocks.post_loop_block);
      break;
    }
    default: { DIE << "bad char '" << instruction << "' at pc=" << pc; }
    }
  }

  if (verbose) {
    builder.CreateCall(dump_memory_func, {memory});
  }

  builder.CreateRetVoid();
  return jit_func;
}

// The main entry to the JIT. Takes the parsed program, emits it to LLVM IR,
// runs optimizations, JITs to native code and runs the native code.
void llvmjit(const Program& program, bool verbose) {
  llvm::LLVMContext context;
  std::unique_ptr<llvm::Module> module(new llvm::Module("bfmodule", context));

  // Add a declaration for external functions used in the JITed code. We use
  // putchar and getchar for I/O and dump_memory for reporting in verbose mode.
  llvm::Type* int32_type = llvm::Type::getInt32Ty(context);
  llvm::Type* void_type = llvm::Type::getVoidTy(context);

  llvm::Function* putchar_func = llvm::Function::Create(
      llvm::FunctionType::get(int32_type, {int32_type}, false),
      llvm::Function::ExternalLinkage, "putchar", module.get());
  llvm::Function* getchar_func = llvm::Function::Create(
      llvm::FunctionType::get(int32_type, {}, false),
      llvm::Function::ExternalLinkage, "getchar", module.get());
  llvm::Function* dump_memory_func = llvm::Function::Create(
      llvm::FunctionType::get(void_type, {llvm::Type::getInt8PtrTy(context)},
                              false),
      llvm::Function::ExternalLinkage, "dump_memory", module.get());

  // Compile the BF program to LLVM IR.
  llvm::Function* jit_func =
      emit_jit_function(program, module.get(), dump_memory_func, putchar_func,
                        getchar_func, verbose);

  if (verbose) {
    const char* pre_opt_file = "/tmp/llvmjit-pre-opt.ll";
    llvm_module_to_file(*module, pre_opt_file);
    std::cout << "[Pre optimization module] dumped to " << pre_opt_file << "\n";
  }

  if (llvm::verifyFunction(*jit_func, &llvm::errs())) {
    DIE << "Error verifying function... exiting";
  }

  // Optimize the emitted LLVM IR.
  Timer topt;

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  llvm::PassManagerBuilder pm_builder;
  pm_builder.OptLevel = 3;
  pm_builder.SizeLevel = 0;
  pm_builder.LoopVectorize = true;
  pm_builder.SLPVectorize = true;

  llvm::legacy::FunctionPassManager function_pm(module.get());
  llvm::legacy::PassManager module_pm;
  pm_builder.populateFunctionPassManager(function_pm);
  pm_builder.populateModulePassManager(module_pm);

  function_pm.doInitialization();
  function_pm.run(*jit_func);
  module_pm.run(*module);

  if (verbose) {
    std::cout << "[Optimization elapsed:] " << topt.elapsed() << "s\n";
    const char* post_opt_file = "/tmp/llvmjit-post-opt.ll";
    llvm_module_to_file(*module, post_opt_file);
    std::cout << "[Post optimization module] dumped to " << post_opt_file
              << "\n";
  }

  // JIT the optimized LLVM IR to native code and execute it.
  SimpleOrcJIT jit(/*verbose=*/verbose);
  module->setDataLayout(jit.get_target_machine().createDataLayout());
  jit.add_module(std::move(module));

  llvm::JITSymbol jit_func_sym = jit.find_symbol(JIT_FUNC_NAME);
  if (!jit_func_sym) {
    DIE << "Unable to find symbol " << JIT_FUNC_NAME << " in module";
  }

  using JitFuncType = void (*)(void);
  JitFuncType jit_func_ptr =
      reinterpret_cast<JitFuncType>(jit_func_sym.getAddress());

  Timer texec;

  jit_func_ptr();

  if (verbose) {
    std::cout << "[-] Execution took: " << texec.elapsed() << "s)\n";
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
