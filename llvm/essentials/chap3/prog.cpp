#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <vector>
#include <string>

static llvm::LLVMContext &Context = llvm::getGlobalContext();
static llvm::Module *ModuleOb = new llvm::Module("my compiler", Context);
static std::vector<std::string> FunArgs;

llvm::Function *createFunc(llvm::IRBuilder<> &Builder, std::string Name) {
  llvm::Type *u32Ty = llvm::Type::getInt32Ty(Context);
  llvm::Type *vecTy = llvm::VectorType::get(u32Ty,2);
  llvm::Type *ptrTy = vecTy->getPointerTo(0);
  llvm::FunctionType *funcType =
      llvm::FunctionType::get(Builder.getInt32Ty(), ptrTy, false);
  llvm::Function *fooFunc = llvm::Function::Create(
      funcType, llvm::Function::ExternalLinkage, Name, ModuleOb);
  return fooFunc;
}

#if 0
typedef llvm::SmallVector<llvm::BasicBlock*, 16> BBList;
typedef llvm::SmallVector<llvm::Value*, 16> ValList;

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
#endif

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

llvm::Value *getGEP(llvm::IRBuilder<> &Builder, llvm::Value *Base,
                    llvm::Value *Offset) {
  return Builder.CreateGEP(Builder.getInt32Ty(), Base, Offset, "a1");
}

llvm::Value *getLoad(llvm::IRBuilder<> &Builder, llvm::Value *Address) {
  return Builder.CreateLoad(Address, "load");
}

void getStore(llvm::IRBuilder<> &Builder, llvm::Value *Address,
              llvm::Value *V) {
  Builder.CreateStore(V, Address);
}

llvm::Value *getInsertElement(llvm::IRBuilder<> &Builder, llvm::Value *Vec, 
    llvm::Value *Val, llvm::Value *Index)
{
  return Builder.CreateInsertElement(Vec, Val, Index);
}

llvm::Value *getExtractElement(llvm::IRBuilder<> &Builder, llvm::Value *Vec,
                               llvm::Value *Index) {
  return Builder.CreateExtractElement(Vec, Index);
}

#if 0
llvm::Value *createIfElse(llvm::IRBuilder<> &Builder, BBList &List,
                          ValList &VL) {
  llvm::Value *Condtn = VL[0];
  llvm::Value *Arg1 = VL[1];
  llvm::Value *Arg2 = VL[2];
  llvm::BasicBlock *ThenBB = List[0];
  llvm::BasicBlock *ElseBB = List[1];
  llvm::BasicBlock *MergeBB = List[2];
  Builder.CreateCondBr(Condtn, ThenBB, ElseBB);
  Builder.SetInsertPoint(ThenBB);

  llvm::Value *ThenVal =
      Builder.CreateAdd(Arg1, Builder.getInt32(1), "thenaddtmp");
  Builder.CreateBr(MergeBB);
  Builder.SetInsertPoint(ElseBB);
  llvm::Value *ElseVal = 
    Builder.CreateAdd(Arg2, Builder.getInt32(2), "elseaddtmp");
  Builder.CreateBr(MergeBB);

  unsigned int PhiBBSize = List.size() - 1;
  Builder.SetInsertPoint(MergeBB);
  llvm::PHINode *Phi = 
    Builder.CreatePHI(llvm::Type::getInt32Ty(Context), PhiBBSize, "iftmp");
  Phi->addIncoming(ThenVal, ThenBB);
  Phi->addIncoming(ElseVal, ElseBB);

  return Phi;
}

llvm::Value *createLoop(llvm::IRBuilder<> &Builder, BBList List, ValList VL,
                          llvm::Value *StartVal, llvm::Value *EndVal) {
  llvm::BasicBlock *PreheaderBB = Builder.GetInsertBlock();
  llvm::Value *val = VL[0];
  llvm::BasicBlock *LoopBB = List[0];
  Builder.CreateBr(LoopBB);
  Builder.SetInsertPoint(LoopBB);
  llvm::PHINode *IndVar =
      Builder.CreatePHI(llvm::Type::getInt32Ty(Context), 2, "i");
  IndVar->addIncoming(StartVal, PreheaderBB);
  llvm::Value *Add = Builder.CreateAdd(val, Builder.getInt32(5), "addtmp");
  llvm::Value *StepVal = Builder.getInt32(1);
  llvm::Value *NextVal = Builder.CreateAdd(IndVar, StepVal, "nextval");
  llvm::Value *EndCond = Builder.CreateICmpULT(IndVar, EndVal, "endcond");
  EndCond = Builder.CreateICmpNE(EndCond, Builder.getInt32(0), "loopcond");
  llvm::BasicBlock *LoopEndBB = Builder.GetInsertBlock();
  llvm::BasicBlock *AfterBB = List[1];
  Builder.CreateCondBr(EndCond, LoopBB, AfterBB);
  Builder.SetInsertPoint(AfterBB);
  IndVar->addIncoming(NextVal, LoopEndBB);
  return Add;
}
#endif

llvm::Value *createArith(llvm::IRBuilder<> &Builder, llvm::Value *L,
                         llvm::Value *R) {
  return Builder.CreateMul(L, R, "multmp");
}

int main(int argc, char *argv[]) {
  FunArgs.push_back("a");
  static llvm::IRBuilder<> Builder(Context);
  
  llvm::Function *fooFunc = createFunc(Builder, "foo");
  setFuncArgs(fooFunc, FunArgs);
  
  llvm::BasicBlock* entry = createBB(fooFunc, "entry");
  Builder.SetInsertPoint(entry);

  llvm::Value *Base = &*fooFunc->arg_begin();
  llvm::SmallVector<llvm::Value*, 4> Vload;
  for (size_t i = 0; i < 4; ++i) {
    Vload[i] = getInsertElement(Builder, Base, Builder.getInt32((i + 1) * 10),
                                Builder.getInt32(i));
  }
  auto gep = getGEP(Builder, Vload[3], Builder.getInt32(1));
  llvm::Value *load = getLoad(Builder, gep);

  llvm::Value *constant = Builder.getInt32(16);
  llvm::Value *val = createArith(Builder, load, constant);
  getStore(Builder, gep, val);

  auto Vec = Base;
  llvm::SmallVector<llvm::Value*, 4> Vvec;
  for (size_t i = 0; i < 4; ++i) {
    Vvec[i] = getExtractElement(Builder, Vec, Builder.getInt32(i));
  }
  auto add1 = createArith(Builder, Vvec[0], Vvec[1]);
  auto add2 = createArith(Builder, add1, Vvec[2]);
  auto add3 = createArith(Builder, add2, Vvec[3]);
  auto add = createArith(Builder, add3, val);

  Builder.CreateRet(add);

#if 0
  FunArgs.push_back("b");
  static llvm::IRBuilder<> Builder(Context);

  llvm::GlobalVariable *gVar = createGlob(Builder, "x");

  llvm::Function *fooFunc = createFunc(Builder, "foo");
  setFuncArgs(fooFunc, FunArgs);
  llvm::BasicBlock* entry = createBB(fooFunc, "entry");
  Builder.SetInsertPoint(entry);

  llvm::Function::arg_iterator arg_it = fooFunc->arg_begin();
  llvm::Value *Arg1 = &*arg_it++;
  llvm::Value *Arg2 = &*arg_it;
  llvm::Value *constant = Builder.getInt32(16);
  llvm::Value *val = createArith(Builder, Arg1, constant);
  llvm::Value *val1 = createArith(Builder, val, gVar);

  llvm::Value *val2 = Builder.getInt32(100);
  llvm::Value *Compare = Builder.CreateICmpULT(val1, val2, "cmptmp");
  llvm::Value *Condtn = Builder.CreateICmpNE(Compare, Builder.getInt32(0), "ifcond");

  ValList VL;
  VL.push_back(Condtn);
  VL.push_back(Arg1);
  VL.push_back(Arg2);

  llvm::BasicBlock *ThenBB = createBB(fooFunc, "then");
  llvm::BasicBlock *ElseBB = createBB(fooFunc, "else");
  llvm::BasicBlock *MergeBB = createBB(fooFunc, "ifcont");

  BBList List;
  List.push_back(ThenBB);
  List.push_back(ElseBB);
  List.push_back(MergeBB);

  llvm::Value *v = createIfElse(Builder, List, VL);

  ValList VL1;
  VL1.push_back(Arg1);

  BBList List1;
  llvm::BasicBlock *LoopBB = createBB(fooFunc, "loop");
  llvm::BasicBlock *AfterBB = createBB(fooFunc, "afterloop");
  List1.push_back(LoopBB);
  List1.push_back(AfterBB);
  llvm::Value *StartVal = v;
  llvm::Value *Res = createLoop(Builder, List1, VL1, StartVal, Arg2);


  Builder.CreateRet(Res);
#endif

  llvm::verifyFunction(*fooFunc);
  ModuleOb->dump();
  return 0;
}
