#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <vector>
#include <string>

static llvm::LLVMContext &Context = llvm::getGlobalContext();
static llvm::Module *ModuleOb = new llvm::Module("my compiler", Context);
static std::vector<std::string> FunArgs;

llvm::Function *createFunc(llvm::IRBuilder<> &Builder, std::string Name)
{
  std::vector<llvm::Type *> Integers(FunArgs.size(),
                                     llvm::Type::getInt32Ty(Context));
  llvm::FunctionType *funcTy =
      llvm::FunctionType::get(Builder.getInt32Ty(), Integers, false);
  llvm::Function *fooFunc = llvm::Function::Create(
      funcTy, llvm::Function::ExternalLinkage, Name, ModuleOb);
  return fooFunc;
}

llvm::GlobalVariable *createGlob(llvm::IRBuilder<> &Builder, std::string Name) {
  ModuleOb->getOrInsertGlobal(Name, Builder.getInt32Ty());
  llvm::GlobalVariable *gVar = ModuleOb->getNamedGlobal(Name);
  gVar->setLinkage(llvm::GlobalValue::CommonLinkage);
  gVar->setAlignment(4);
  return gVar;
}

llvm::BasicBlock *createBB(llvm::Function *fooFunc, std::string Name)
{
  return llvm::BasicBlock::Create(Context, Name, fooFunc);
}

void setFuncArgs(llvm::Function *fooFunc, std::vector<std::string> &FunArgs) {
  unsigned int Idx = 0;
  for (llvm::Function::arg_iterator AI = fooFunc->arg_begin(),
                                    AE = fooFunc->arg_end();
       AI != AE; ++AI, ++Idx) {
    AI->setName(FunArgs[Idx]);
  }
}

llvm::Value *createArith(llvm::IRBuilder<> &Builder, llvm::Value *L,
                         llvm::Value *R) {
  return Builder.CreateMul(L, R, "multmp");
}

int main(int argc, char *argv[]) {
  FunArgs.push_back("a");
  FunArgs.push_back("b");
  static llvm::IRBuilder<> Builder(Context);

  llvm::GlobalVariable *gVar = createGlob(Builder, "x");

  llvm::Function *fooFunc = createFunc(Builder, "foo");
  setFuncArgs(fooFunc, FunArgs);
  llvm::BasicBlock* entry = createBB(fooFunc, "entry");
  Builder.SetInsertPoint(entry);

  llvm::Value *Arg1 = &*fooFunc->arg_begin();
  llvm::Value *constant = Builder.getInt32(16);
  llvm::Value *val = createArith(Builder, Arg1, constant);
  llvm::Value *val1 = createArith(Builder, val, gVar);

  //Builder.CreateRet(Builder.getInt32(0));
  Builder.CreateRet(val1);

  llvm::verifyFunction(*fooFunc);
  ModuleOb->dump();
  return 0;
}
