#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <vector>
#include <string>

static llvm::LLVMContext &Context = llvm::getGlobalContext();
static llvm::Module *ModuleOb = new llvm::Module("my compiler", Context);

llvm::Function *createFunc(llvm::IRBuilder<> &Builder, std::string Name)
{
  llvm::FunctionType *funcTy = llvm::FunctionType::get(Builder.getInt32Ty(), false);
  llvm::Function *fooFunc = llvm::Function::Create(
      funcTy, llvm::Function::ExternalLinkage, Name, ModuleOb);
  return fooFunc;
}

int main(int argc, char *argv[]) {
  static llvm::IRBuilder<> Builder(Context);
  llvm::Function *fooFunc = createFunc(Builder, "foo");
  llvm::verifyFunction(*fooFunc);
  ModuleOb->dump();
  return 0;
}
