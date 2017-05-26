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
 

  //------------------- Basic Block -------------------------------------------

  struct BasicBlock {
    LLVMCodeGen &CG;
    llvm::BasicBlock &BB;
    BasicBlock(LLVMCodeGen &cg, llvm::BasicBlock &bb) : CG(cg), BB(bb) {}
    llvm::BasicBlock *get() { return &BB; }
    void set() { CG.Builder.SetInsertPoint(&BB); }

  };

  // -----------------------  Boolean -------------------------------------------

  struct Boolean {
    LLVMCodeGen *CG;
    llvm::Value *V;

    Boolean(LLVMCodeGen *cg, llvm::Value *value) : CG(cg), V(value) {}

    llvm::Value *get() { return V; }

    void mkIfThenElse(BasicBlock thenBB, BasicBlock elseBB,
                      BasicBlock mergeBB) {
      auto bb = CG->Builder.GetInsertBlock();
      CG->Builder.CreateCondBr(V, thenBB.get(), elseBB.get());
      CG->Builder.SetInsertPoint(thenBB.get());
      CG->Builder.CreateBr(mergeBB.get());
      CG->Builder.SetInsertPoint(elseBB.get());
      CG->Builder.CreateBr(mergeBB.get());
      CG->Builder.SetInsertPoint(bb);
    }
  };
  
  // -----------------------  Value -------------------------------------------
  struct Value {
    LLVMCodeGen *CG;
    llvm::Value *V;

    Value(LLVMCodeGen *cg, llvm::Value *value) : CG(cg), V(value) {}

    llvm::Value *get() { return V; }

    Value operator*(Value R) {
      return {CG, CG->Builder.CreateMul(V, R.get(), "mul")};
    }
    Value operator+(Value R) {
      return {CG, CG->Builder.CreateAdd(V, R.get(), "add")};
    }

    Value operator<(Value R) {
      return {CG, CG->Builder.CreateICmpULT(V, R.get(), "lt")};
    }
    Boolean operator!=(Value R) {
      return {CG, CG->Builder.CreateICmpNE(V, R.get(), "ne")};
    }
  };

  //----------------------- Type ---------------------------------------------

  struct Type {
    enum Kind { int32, float32, float64 };

    LLVMCodeGen &CG;
    Kind TypeKind;

    Type(LLVMCodeGen &cg, Kind type_kind) : CG(cg), TypeKind(type_kind) {}

    llvm::Type *get() {
      switch (TypeKind) {
      case int32:
        return CG.Builder.getInt32Ty();
      case float32:
        return CG.Builder.getFloatTy();
      case float64:
        return CG.Builder.getDoubleTy();
      default:
        assert(0 && "Must not happen");
      }
    }

    size_t size() {
      switch (TypeKind) {
      case int32:
      case float32:
        return 4;
      case float64:
        return 8;
      default:
        assert(0 && "Must not happen");
      }
      return 0;
    }
  };

  // --------------------- Global Variable -----------------------------------
  
  struct GlobalVar {
    LLVMCodeGen &CG;
    llvm::GlobalVariable &Var;
    GlobalVar(LLVMCodeGen &cg, llvm::GlobalVariable &var)
        : CG(cg), Var(var) {}
  };

  GlobalVar mkGlobalVar(std::string name, Type type) {
    M.getOrInsertGlobal(name, type.get());
    llvm::GlobalVariable *gvar = M.getNamedGlobal(name);
    gvar->setLinkage(llvm::GlobalValue::CommonLinkage);
    gvar->setAlignment(type.size());
    return GlobalVar{*this, *gvar};
  }

  //---------------------- Function -------------------------------------------
  
  struct Function {
    LLVMCodeGen &CG;
    llvm::Function &F;
    std::vector<llvm::Value*> Args;
    Function(LLVMCodeGen &cg, llvm::Function &f) : CG(cg), F(f) {
      Args.reserve(16);
      for (auto &arg : F.args()) {
        Args.push_back(&arg);
      }
    }
    void verify() const { llvm::verifyFunction(F); }

    llvm::Function *get() { return &F; }

    Value arg(size_t num) {
      assert(num < Args.size());
      return Value{&CG, Args[num]};
    }

    BasicBlock mkBasicBlock(std::string name) {
      llvm::BasicBlock *bb = llvm::BasicBlock::Create(CG.Context, name, &F);
      return {CG, *bb};
    }
  };

  Function mkFunction(std::string name, Type ret_type,
                      std::vector<std::pair<Type, std::string>> args = {}) {
    std::vector<llvm::Type*> args_type;
    args_type.reserve(args.size());
    for (auto arg : args) {
      args_type.push_back(arg.first.get());
    }
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(ret_type.get(), args_type, false);
    llvm::Function *func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, name, &M);
    int count = 0;
    for (auto &arg : func->args()) {
      arg.setName(args[count++].second);
    }
    return Function{*this, *func};
  }


  // --------------------------- Phi node ------------------------------------
  struct Phi {
    LLVMCodeGen *CG;
    llvm::Value *V;

    Phi(LLVMCodeGen *cg, llvm::Value *v) : CG(cg), V(v) {}
    llvm::Value *get() { return V; }

    operator Value() { return {CG, V}; }
  };

  Phi mkPhi(Type type, std::vector<std::pair<Value, BasicBlock>> edge_list) {
    llvm::PHINode *phi = Builder.CreatePHI(type.get(), edge_list.size());
    for (auto &edge : edge_list) {
      phi->addIncoming(edge.first.get(), edge.second.get());
    }
    return {this, phi};
  }

  // ------------- type & vals
 
  Type mkIntTy() { return Type(*this, Type::int32); }
  Value mkInt(int val) { return {this, Builder.getInt32(val)}; }
  Type mkFloatTy() { return Type(*this, Type::float32); }
  Type mkDoubleTy() { return Type(*this, Type::float64); }

  //-------------------- Ops

  void mkRet(Value val) { Builder.CreateRet(val.get()); }

  // ---------------- loop
  void mkLoop(Value begin, Value end, Value step, Value iv, BasicBlock loopBB,
              BasicBlock afterBB) {
    auto preheaderBB = Builder.GetInsertBlock();
  }
};

static llvm::LLVMContext &ContextRef = llvm::getGlobalContext();
static llvm::Module *ModuleOb = new llvm::Module("my compiler", ContextRef);

int main(int argc, char *argv[]) {
  static llvm::IRBuilder<> BuilderObj(ContextRef);
  LLVMCodeGen cg(ContextRef, *ModuleOb, BuilderObj);

  auto gvar = cg.mkGlobalVar("x", cg.mkFloatTy());

  auto f = cg.mkFunction("foo", cg.mkIntTy(),
                         {{cg.mkIntTy(), "a"}, {cg.mkFloatTy(), "b"}});

  auto entryBB = f.mkBasicBlock("entry");
  auto thenBB = f.mkBasicBlock("then");
  auto elseBB = f.mkBasicBlock("else");
  auto mergeBB = f.mkBasicBlock("cont");

  entryBB.set();
  auto val = cg.mkInt(100);
  auto cmp = f.arg(0) < val;
  auto cnd = cmp != cg.mkInt(0);
  cnd.mkIfThenElse(thenBB, elseBB, mergeBB);

  thenBB.set();
  auto then_val = f.arg(0) + cg.mkInt(1);

  elseBB.set();
  auto else_val = f.arg(1) + cg.mkInt(2);

  mergeBB.set();
  auto phi = cg.mkPhi(cg.mkIntTy(), {{then_val, thenBB}, {else_val, elseBB}});
  cg.mkRet(phi);


  f.verify();

  cg.dump();
  return 0;
}
