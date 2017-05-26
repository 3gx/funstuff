#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <vector>
#include <string>


struct LLVMCodeGen {
  llvm::LLVMContext &Context;
  llvm::Module &M;
  llvm::IRBuilder<> &Builder;

  LLVMCodeGen(llvm::LLVMContext &context, llvm::Module &m,
              llvm::IRBuilder<> &builder)
      : Context(context), M(m), Builder(builder) {}
  void dump() const { M.dump(); }

  struct Function {
    llvm::Function &F;
    Function(llvm::Function &f) : F(f) {}
    void verify() const { llvm::verifyFunction(F); }
  };

  //---------------------------------------------------------------------------

  llvm::Type *int32() const { return Builder.getInt32Ty(); }

  Function mkFunction(std::string fname) {
    llvm::FunctionType *funcType = llvm::FunctionType::get(int32(), false);
    llvm::Function *func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, fname, &M);
    return Function{*func};
  }
};

static llvm::LLVMContext &ContextRef = llvm::getGlobalContext();
static llvm::Module *ModuleOb = new llvm::Module("my compiler", ContextRef);

int main(int argc, char *argv[]) {
  static llvm::IRBuilder<> BuilderObj(ContextRef);
  LLVMCodeGen cg(ContextRef, *ModuleOb, BuilderObj);
  auto f = cg.mkFunction("foo");
  f.verify();
  cg.dump();
  return 0;
}
