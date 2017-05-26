#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <vector>
#include <string>

struct LLVMCodeGen {
  using IRBuilder = llvm::IRBuilder<>;
  llvm::LLVMContext &Context;
  llvm::Module &M;
  IRBuilder &Builder;

  LLVMCodeGen(llvm::LLVMContext &context, llvm::Module &m,
              llvm::IRBuilder<> &builder)
      : Context(context), M(m), Builder(builder) {}
  void dump() const { M.dump(); }


  //---------------------------------------------------------------------------

  llvm::Type *int32() const { return Builder.getInt32Ty(); }
  
  //---------------------- Function -------------------------------------------
  
  struct Function {
    llvm::Function &F;
    Function(llvm::Function &f) : F(f) {}
    void verify() const { llvm::verifyFunction(F); }

    operator llvm::Function *() { return &F; }
  };

  Function mkFunction(std::string name) {
    llvm::FunctionType *funcType = llvm::FunctionType::get(int32(), false);
    llvm::Function *func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, name, &M);
    return Function{*func};
  }


  //------------------- Basic Block -------------------------------------------
  struct BasicBlock {
    IRBuilder &Builder;
    llvm::BasicBlock &BB;
    BasicBlock(IRBuilder &builder, llvm::BasicBlock &bb)
        : Builder(builder), BB(bb) {}
    operator llvm::BasicBlock *() { return &BB; }
    void set() { Builder.SetInsertPoint(&BB); }
  };

  BasicBlock mkBasicBlock(Function &func, std::string name) {
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(Context, name, func);
    return BasicBlock{Builder, *bb};
  }


};

static llvm::LLVMContext &ContextRef = llvm::getGlobalContext();
static llvm::Module *ModuleOb = new llvm::Module("my compiler", ContextRef);

int main(int argc, char *argv[]) {
  static llvm::IRBuilder<> BuilderObj(ContextRef);
  LLVMCodeGen cg(ContextRef, *ModuleOb, BuilderObj);
  auto f = cg.mkFunction("foo");

  auto entry = cg.mkBasicBlock(f, "entry");
  entry.set();
  f.verify();

  cg.dump();
  return 0;
}
