#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <vector>

static llvm::LLVMContext &Context = llvm::getGlobalContext();
static llvm::Module *ModuleOb = new llvm::Module("my compiler", Context);

int main(int argc, char *argv[]) {
  ModuleOb->dump();
  return 0;
}
