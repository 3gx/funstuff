#ifdef NDEBUG
#undef NDEBUG
#endif

#include "LLVMIRBuilder.hpp"

#if 0
struct LLVMCodeGen {
  using IRBuilder = llvm::IRBuilder<>;
  llvm::LLVMContext &Context;
  llvm::Module &M;
  IRBuilder &Builder;

  LLVMCodeGen(llvm::LLVMContext &context, llvm::Module &m,
              llvm::IRBuilder<> &builder)
      : Context(context), M(m), Builder(builder) {}
  void dump() const { M.dump(); }
  
  //----------------------- Type ---------------------------------------------

  struct Type {
    enum Kind { int32, float32, float64 };

    LLVMCodeGen *CG;
    Kind TypeKind;

    Type(LLVMCodeGen *cg, Kind type_kind) : CG(cg), TypeKind(type_kind) {}
    Type(LLVMCodeGen *cg, llvm::Type *ty) : CG(cg) {
      if (ty->getTypeID() == CG->Builder.getInt32Ty()->getTypeID()) {
        TypeKind = int32;
      } else if (ty->getTypeID() == CG->Builder.getFloatTy()->getTypeID()) {
        TypeKind = float32;
      } else if (ty->getTypeID() == CG->Builder.getDoubleTy()->getTypeID()) {
        TypeKind = float64;
      } else {
        assert(0 && "Must not happend");
      }
    }

    llvm::Type *get() {
      switch (TypeKind) {
      case int32:
        return CG->Builder.getInt32Ty();
      case float32:
        return CG->Builder.getFloatTy();
      case float64:
        return CG->Builder.getDoubleTy();
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
 

  //------------------- Basic Block -------------------------------------------

  struct Function;
  struct BasicBlock {
    LLVMCodeGen *CG;
    llvm::BasicBlock *BB;
    BasicBlock(LLVMCodeGen *cg, llvm::BasicBlock *bb) : CG(cg), BB(bb) {}
    llvm::BasicBlock *get() { return BB; }
    void set() { CG->Builder.SetInsertPoint(BB); }
    Function getParent();
  };

  // -----------------------  Boolean -------------------------------------------

  struct Value;

  struct Boolean {
    LLVMCodeGen *CG;
    llvm::Value *V;

    Boolean(LLVMCodeGen *cg, llvm::Value *value) : CG(cg), V(value) {}

    llvm::Value *get() { return V; }

    void mkIfThenElse(BasicBlock thenBB, BasicBlock elseBB) {
      CG->Builder.CreateCondBr(V, thenBB.get(), elseBB.get());
    }
    Value mkSelect(Value thenV, Value elseV);
  };
  
  // -----------------------  Value -------------------------------------------
  struct Value {
    LLVMCodeGen *CG;
    llvm::Value *V;

    Value(LLVMCodeGen *cg, llvm::Value *value) : CG(cg), V(value) {}
    Type getType() { return {CG, V->getType()}; }


    llvm::Value *get() { return V; }

    Value operator*(Value R) {
      return {CG, CG->Builder.CreateMul(V, R.get(), "mul")};
    }
    Value operator+(Value R) {
      return {CG, CG->Builder.CreateAdd(V, R.get(), "add")};
    }

    Boolean operator<(Value R) {
      return {CG, CG->Builder.CreateICmpULT(V, R.get(), "lt")};
    }
    Boolean operator!=(Value R) {
      return {CG, CG->Builder.CreateICmpNE(V, R.get(), "ne")};
    }
  };


  struct AllocaValue {
    LLVMCodeGen *CG;
    llvm::Value *V;

    explicit AllocaValue(Value v, void*) : CG(v.CG) {
      V = CG->Builder.CreateAlloca(v.get()->getType());
      store(v);
    }

    Value load() {
      auto v = CG->Builder.CreateLoad(V);
      return {CG, v};
    }
    void store(Value v) { CG->Builder.CreateStore(v.get(), V); }

    AllocaValue& operator*=(Value R) { store(load() * R); return *this; }
    AllocaValue& operator+=(Value R) { store(load() + R); return *this; }
  };

  AllocaValue mkAlloca(Value v) { return AllocaValue(v, nullptr); }

#if 0
  // --------------------- Global Variable -----------------------------------
  
  struct GlobalVar {
    LLVMCodeGen *CG;
    llvm::GlobalVariable *Var;
    GlobalVar(LLVMCodeGen *cg, llvm::GlobalVariable *var)
        : CG(cg), Var(var) {}
  };

  GlobalVar mkGlobalVar(std::string name, Type type) {
    M.getOrInsertGlobal(name, type.get());
    llvm::GlobalVariable *gvar = M.getNamedGlobal(name);
    gvar->setLinkage(llvm::GlobalValue::CommonLinkage);
    gvar->setAlignment(type.size());
    return GlobalVar{this, gvar};
  }
#endif

  //---------------------- Function -------------------------------------------
  
  struct Function {
    LLVMCodeGen *CG;
    llvm::Function *F;
    std::vector<llvm::Value*> Args;
    Function(LLVMCodeGen *cg, llvm::Function *f) : CG(cg), F(f) {
      Args.reserve(16);
      for (auto &arg : F->args()) {
        Args.push_back(&arg);
      }
    }
    std::string ErrorString;
    bool verify() { 
      ErrorString.clear();
      llvm::raw_string_ostream os(ErrorString);
      return llvm::verifyFunction(*F, &os); 
    }
    size_t n_args() const { return Args.size(); }

    llvm::Function *get() { return F; }
    std::string const& getErrorString() { return ErrorString;}
    std::string getName() const { return std::string(F->getName().data()); }

    Value arg(size_t num) {
      assert(num < Args.size());
      return Value{CG, Args[num]};
    }

    BasicBlock mkBasicBlock(std::string name) {
      llvm::BasicBlock *bb = llvm::BasicBlock::Create(CG->Context, name, F);
      return {CG, bb};
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
    return Function{this, func};
  }
  Function mkFunction(std::string name,
                      std::vector<std::pair<Type, std::string>> args = {}) {
    std::vector<llvm::Type*> args_type;
    args_type.reserve(args.size());
    for (auto arg : args) {
      args_type.push_back(arg.first.get());
    }
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(Builder.getVoidTy(), args_type, false);
    llvm::Function *func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, name, &M);
    int count = 0;
    for (auto &arg : func->args()) {
      arg.setName(args[count++].second);
    }
    return Function{this, func};
  }

  // -------------------------- Call ------------------------------------------
  struct CallInst {
    LLVMCodeGen *CG;
    llvm::CallInst *Inst;
    CallInst(LLVMCodeGen *cg, llvm::CallInst *inst) : CG(cg), Inst(inst) {}

    operator Value() { return {CG, Inst}; }
    llvm::CallInst *get() { return Inst; }
  };

  CallInst mkCall(Function &f, std::vector<Value> args) {
    assert(args.size() == f.n_args() && "Argument count mismatch");
    std::vector<llvm::Value*> arguments;
    arguments.reserve(args.size());
    for (auto &arg : args) {
      arguments.push_back(arg.get());
    }
    return {this, Builder.CreateCall(f.get(), arguments)};
  }

  // --------------------------- Phi node ------------------------------------
  struct Phi {
    LLVMCodeGen *CG;
    llvm::PHINode *V;

    Phi(LLVMCodeGen *cg, llvm::PHINode *v) : CG(cg), V(v) {}
    llvm::PHINode *get() { return V; }

    operator Value() { return {CG, V}; }

    Phi &operator+=(std::pair<Value, BasicBlock> rhs) {
      V->addIncoming(rhs.first.get(), rhs.second.get());
      return *this;
    }
  };

  Phi mkPhi(Type type, std::vector<std::pair<Value, BasicBlock>> edge_list) {
    llvm::PHINode *phi = Builder.CreatePHI(type.get(), edge_list.size());
    for (auto &edge : edge_list) {
      phi->addIncoming(edge.first.get(), edge.second.get());
    }
    return {this, phi};
  }

  // ------------- type & vals
 
  Type mkIntTy() { return Type(this, Type::int32); }
  Value mkInt(int val) { return {this, Builder.getInt32(val)}; }
  Type mkFloatTy() { return Type(this, Type::float32); }
  Type mkDoubleTy() { return Type(this, Type::float64); }

  BasicBlock getCurrentBasicBlock() { return {this, Builder.GetInsertBlock()}; }

  //-------------------- Ops

  void mkRet(Value val) { Builder.CreateRet(val.get()); }
  void mkRetVoid() { Builder.CreateRetVoid(); }

  // ---------------- loop

  template <class F>
  Value mkLoop(Value begin, Value end, Value step, F body) {
    auto bb = getCurrentBasicBlock();
    auto preBB = bb.getParent().mkBasicBlock("preBB");
    auto loopBB = bb.getParent().mkBasicBlock("loopBB");
    auto afterBB = bb.getParent().mkBasicBlock("afterBB");
    auto iv = mkAlloca(begin);
    mkBranch(preBB);

    preBB.set();
    auto cond = iv.load() < end;
    cond.mkIfThenElse(loopBB, afterBB);

    loopBB.set();
    body(iv.load());
    iv += step;
    mkBranch(preBB);

    afterBB.set();
    return iv.load();
  }

  template <class F>
  void mkLoopImpl(std::vector<Value> begs, std::vector<Value> ends,
                   std::vector<Value> steps, F body, std::vector<Value> &ivs,
                   std::vector<Value> &end_ivs,
                   std::vector<BasicBlock> &end_bbs) {
    // must be matching sizes
    assert(begs.size() == ends.size());
    assert(begs.size() == steps.size());

    // if we proceeded all indices, just emplace body of the loop, and return
    if (begs.empty()) {

      // since ivs are stored in reverse, so reverse the list :)
      std::reverse(ivs.begin(), ivs.end());
      
      // emplace function body
      body(ivs);
      return;
    }

    // otherwise, process next index in reverse order
    auto beg = begs.back();
    auto end = ends.back();
    auto step = steps.back();
    begs.pop_back();
    ends.pop_back();
    steps.pop_back();

    // make 1d loop, which recursively calls this function with 1 index less
    auto ret = mkLoop(beg, end, step, [&](Value iv) {
      ivs.push_back(iv);
      mkLoopImpl(begs, ends, steps, body, ivs, end_ivs, end_bbs);
    });
    auto bb = getCurrentBasicBlock();
    end_ivs.push_back(ret);
    end_bbs.push_back(bb);
  }

  template <class F>
  std::vector<Value> mkLoop(std::vector<Value> begs, std::vector<Value> ends,
                             std::vector<Value> steps, F body) {
    assert(!begs.empty());


    // since impl iterates from the last index to the first, so invert the list
    std::reverse(begs.begin(), begs.end());
    std::reverse(ends.begin(), ends.end());
    std::reverse(steps.begin(), steps.end());
    
    // ending inductin variable list
    std::vector<Value> ivs, end_ivs;
    std::vector<BasicBlock> end_bbs;
    ivs.reserve(begs.size());
    end_ivs.reserve(begs.size());
    end_bbs.reserve(begs.size());

    // generate the loop
    mkLoopImpl(begs, ends, steps, body, ivs, end_ivs, end_bbs);
    std::reverse(end_ivs.begin(), end_ivs.end());
    end_bbs.back().set();
    
    return end_ivs;
  }

  // ---------------- branchInst
  struct BranchInst {
    LLVMCodeGen *CG;
    llvm::BranchInst *Inst;
    BranchInst(LLVMCodeGen *cg, llvm::BranchInst *inst) : CG(cg), Inst(inst) {}

    operator Value() { return {CG, Inst}; }
    llvm::BranchInst *get() { return Inst; }
  };

  BranchInst mkBranch(BasicBlock& bb) {
    return {this, Builder.CreateBr(bb.get())};
    }

    bool verifyModule() { return llvm::verifyModule(M); }

};
LLVMCodeGen::Function LLVMCodeGen::BasicBlock::getParent() {
  return {CG, BB->getParent()};
}
LLVMCodeGen::Value LLVMCodeGen::Boolean::mkSelect(Value thenV, Value elseV) {
  return {CG, CG->Builder.CreateSelect(V, thenV.get(), elseV.get())};
}
#endif

static llvm::LLVMContext &ContextRef = llvm::getGlobalContext();
static llvm::Module *ModuleOb = new llvm::Module("my compiler", ContextRef);

int main(int argc, char *argv[]) {
//  static llvm::IRBuilder<> BuilderObj(ContextRef);
//  LLVMCodeGen cg(ContextRef, *ModuleOb, BuilderObj);

  LLVMCodegen::IRBuilder cg(*ModuleOb);

#if 0
  auto gvar = cg.mkGlobalVar("x", cg.mkFloatTy());
#endif

  auto f = cg.mkFunction("foo", cg.mkIntTy(),
                         {{cg.mkIntTy(), "a"}, {cg.mkIntTy(), "b"}});

  auto entryBB = f.mkBasicBlock("entry");
  entryBB.set();
  auto thenBB = f.mkBasicBlock("then");
  auto elseBB = f.mkBasicBlock("else");
  auto mergeBB = f.mkBasicBlock("cont");

  entryBB.set();
  auto res = cg.mkAlloca(cg.mkInt(0));
  auto val = cg.mkInt(100);
  auto cnd = f.arg(0) < val;
  cnd.mkIfThenElse(thenBB, elseBB);

  thenBB.set();
  auto then_val = f.arg(0) + cg.mkInt(1);
  res.store(then_val);
  cg.mkBranch(mergeBB);
  

  elseBB.set();
  auto else_val = f.arg(1) + cg.mkInt(2);
  res.store(else_val);
  cg.mkBranch(mergeBB);

  mergeBB.set();
  auto phi = res.load();

  auto f1 = [&](LLVMCodegen::Value iv) { res += iv + phi; };
  auto last = cg.mkLoop(cg.mkInt(0),  phi, cg.mkInt(1), f1);
  auto cmp = last != cg.mkInt(32);

  auto sum = cg.mkAlloca(cg.mkInt(0));
#if 1
  auto last1 = cg.mkLoop(
      {cg.mkInt(0), cg.mkInt(0)}, {cg.mkInt(4), cg.mkInt(5)},
      {cg.mkInt(1), cg.mkInt(1)},
      [&](std::vector<LLVMCodegen::Value> iv) { sum += iv[0] * iv[1]; });
#else
  auto last1 = cg.mkLoop({cg.mkInt(0)}, {cg.mkInt(15)}, {cg.mkInt(1)},
                         [&](LLVMCodegen::Value *iv) { sum += iv[0]; });
#endif

  cg.mkRet(cmp.mkSelect(last, sum.load()));
  cg.dump();

  int error = 0;
  if (f.verify()) {
    std::cerr << "\n";
    std::cerr << "============================================================\n";
    std::cerr << "Function <" << f.getName() << "> verification error\n";
    std::cerr << "============================================================\n";
    std::cerr << f.getErrorString();
    std::cerr << "------------------------------------------------------------\n";
    error = 1;
  }
//  assert(!cg.verifyModule());

  return error;
}
