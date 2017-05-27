#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <vector>
#include <string>
#include <iostream>

namespace LLVMCodegen {

struct Type;
struct BasicBlock;
struct Boolean;
struct Value;
struct AllocaValue;
struct Function;
struct CallInst;
struct PhiNode;
struct BranchInst;

struct IRBuilder {
private:
  using LLVMIRBuilder = llvm::IRBuilder<>;

  llvm::Module& Module;
  std::unique_ptr<LLVMIRBuilder> Builder;

public:
  explicit IRBuilder(llvm::Module& module) : Module(module) {
    Builder.reset(new LLVMIRBuilder(getContext()));
  }

  llvm::LLVMContext& getContext() {
    return Module.getContext();
  }

  llvm::Module& getModule() { return Module; }

  LLVMIRBuilder* operator->() { return Builder.get(); }
  
  void dump() const { Module.dump(); }
  bool verifyModule() { return llvm::verifyModule(Module); }

  // -- method declarations

  // -- Type 

  Type mkIntTy();
  Type mkFloatTy();
  Type mkDoubleTy();
  
  // -- BasicBlock

  BasicBlock getCurrentBasicBlock();

  // -- Value

  Value mkInt(int);

  // -- AllocaValue
 
  AllocaValue mkAlloca(Value value);

  // -- Function

private:
  Function mkFunctionImpl(std::string name,
                          std::vector<Type> ret_type,
                          std::vector<std::pair<Type, std::string>> args);

public:
  Function mkVoidFunction(std::string name,
                          std::vector<std::pair<Type, std::string>> args = {});
  Function mkFunction(std::string name, Type ret_type,
                      std::vector<std::pair<Type, std::string>> args = {});

  // -- CallInst

  CallInst mkCall(Function fun, std::vector<Value> args);
  
  // -- BranchInst
  
  BranchInst mkBranch(BasicBlock);
  
  // -- PhiNode

  PhiNode mkPhiNode(Type type,
                    std::vector<std::pair<Value, BasicBlock>> edge_list);

  // -- Ret
  
  void mkRet(Value val);
  void mkRetVoid();

  // -- Loops
  
  template <class F>
  Value mkLoop(Value begin, Value end, Value step, F body);
  template <class F>
  std::vector<Value> mkNdLoop(
      std::vector<std::tuple<Value, Value, Value>> trip_count,
      F body);

}; // struct IRBuilder

struct IRBuilderRef {
  IRBuilder& Builder;
  IRBuilderRef(IRBuilder& builder) : Builder(builder){};
  IRBuilderRef& operator=(IRBuilderRef const&) { return *this; }
}; // struct BRef

//---------------------------- Type -------------------------------------------

struct Type : IRBuilderRef {

  enum Kind { int32,
              float32,
              float64 };

  Kind TypeKind;
  Type(IRBuilder& builder, Kind type_kind) : IRBuilderRef(builder),
                                             TypeKind(type_kind){};
  Type(IRBuilder& builder, llvm::Type* type) : IRBuilderRef(builder) {
    if (type->getTypeID() == Builder->getInt32Ty()->getTypeID()) {
      TypeKind = int32;
    } else if (type->getTypeID() == Builder->getFloatTy()->getTypeID()) {
      TypeKind = float32;
    } else if (type->getTypeID() == Builder->getDoubleTy()->getTypeID()) {
      TypeKind = float64;
    } else {
      assert(0 && "Must not happend");
    }
  }

  llvm::Type* get() {
    switch (TypeKind) {
    case int32:
      return Builder->getInt32Ty();
    case float32:
      return Builder->getFloatTy();
    case float64:
      return Builder->getDoubleTy();
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
}; // struct Type
  
//--------------------- Basic Block -------------------------------------------

// -- Function forward declaration
//    required since BasicBlock can return its parent function scope
struct BasicBlock : IRBuilderRef {
  llvm::BasicBlock* BB;
  BasicBlock(IRBuilder& builder, llvm::BasicBlock* bb) : IRBuilderRef(builder),
  BB(bb) {}
  llvm::BasicBlock* get() { return BB; }
  void set() { Builder->SetInsertPoint(BB); }

  // -- declarations
  Function getParent();
}; // struct BasicBlock

// -----------------------  Boolean -------------------------------------------
  
// -- Value forward delcaration
//    required since Boolean returns Value when applying select operation
struct Boolean : IRBuilderRef {
  llvm::Value* V;

  Boolean(IRBuilder& ref, llvm::Value* value) : IRBuilderRef(ref), V(value) {}

  llvm::Value* get() { return V; }

  void mkIfThenElse(BasicBlock thenBB, BasicBlock elseBB) {
    Builder->CreateCondBr(V, thenBB.get(), elseBB.get());
  }

  // -- declaration
  Value mkSelect(Value thenV, Value elseV);
}; // struct Boolean

// -----------------------  Value -------------------------------------------

struct Value : IRBuilderRef {
  llvm::Value* V;

  Value(IRBuilder& ref, llvm::Value* value) : IRBuilderRef(ref), V(value) {}
  Type getType() { return {Builder, V->getType()}; }

  llvm::Value* get() { return V; }

  Value operator*(Value R) {
    return {Builder, Builder->CreateMul(this->get(), R.get(), "mul")};
  }
  Value operator+(Value R) {
    return {Builder, Builder->CreateAdd(this->get(), R.get(), "add")};
  }

  Boolean operator<(Value R) {
    return {Builder, Builder->CreateICmpULT(this->get(), R.get(), "lt")};
  }
  Boolean operator!=(Value R) {
    return {Builder, Builder->CreateICmpNE(this->get(), R.get(), "ne")};
  }
}; // struct Value
Value Boolean::mkSelect(Value thenV, Value elseV) {
  return {Builder, Builder->CreateSelect(V, thenV.get(), elseV.get())};
}

// ------------------------ AllocaValue ---------------------------------------

struct AllocaValue : IRBuilderRef {
  friend AllocaValue IRBuilder::mkAlloca(Value);
private:
  llvm::Value* V;

  explicit AllocaValue(Value v) : IRBuilderRef(v.Builder) {
    V = Builder->CreateAlloca(v.get()->getType());
    store(v);
  }

public:
  Value load() {
    return {Builder, Builder->CreateLoad(V)};
  }
  void store(Value v) { Builder->CreateStore(v.get(), V); }

  AllocaValue& operator*=(Value R) {
    store(load() * R);
    return *this;
  }
  AllocaValue& operator+=(Value R) {
    store(load() + R);
    return *this;
  }
}; // struct AllocaValue

//---------------------- Function -------------------------------------------

struct Function : IRBuilderRef {

  llvm::Function* F;
  std::vector<llvm::Value*> Args;
  std::string ErrorString;

  Function(IRBuilder& ref, llvm::Function* f) : IRBuilderRef(ref), F(f) {
    Args.reserve(16);
    for (auto& arg : F->args()) {
      Args.push_back(&arg);
    }
  }
  size_t n_args() const { return Args.size(); }
  bool verify() {
    ErrorString.clear();
    llvm::raw_string_ostream os(ErrorString);
    return llvm::verifyFunction(*F, &os);
  }

  llvm::Function* get() { return F; }
  std::string const& getErrorString() { return ErrorString; }
  std::string getName() const { return std::string(F->getName().data()); }

  Value arg(size_t num) {
    assert(num < Args.size());
    return Value{Builder, Args[num]};
  }

  BasicBlock mkBasicBlock(std::string name) {
    return {Builder, llvm::BasicBlock::Create(Builder.getContext(), name, F)};
  }
}; // struct Function
Function BasicBlock::getParent() { return {Builder, BB->getParent()}; }

// -------------------------- CallInst ----------------------------------------

struct CallInst : IRBuilderRef {
  llvm::CallInst* Inst;
  CallInst(IRBuilder& ref, llvm::CallInst* inst) : IRBuilderRef(ref),
                                                   Inst(inst) {}

  explicit operator Value() { return {Builder, Inst}; }
  llvm::CallInst* get() { return Inst; }
}; // struct CallInst

// -------------------------- BranchInst ------------------------------------

struct BranchInst : IRBuilderRef {
  llvm::BranchInst* Inst;
  BranchInst(IRBuilder& ref, llvm::BranchInst* inst) : IRBuilderRef(ref),
                                                       Inst(inst) {}
  explicit operator Value() { return {Builder, Inst}; }
  llvm::BranchInst* get() { return Inst; }
};

// ------------------------------- PhiNode ------------------------------------

struct PhiNode : IRBuilderRef {
  llvm::PHINode* V;

  PhiNode(IRBuilder& ref, llvm::PHINode* v) : IRBuilderRef(ref), V(v) {}
  llvm::PHINode* get() { return V; }

  explicit operator Value() { return {Builder, V}; }

  PhiNode& operator+=(std::pair<Value, BasicBlock> rhs) {
    V->addIncoming(rhs.first.get(), rhs.second.get());
    return *this;
  }
};


// -- method definitions
  
// -- Type 

Type IRBuilder::mkIntTy() { return {*this, Type::int32}; }
Type IRBuilder::mkFloatTy() { return {*this, Type::float32}; }
Type IRBuilder::mkDoubleTy() { return {*this, Type::float64}; }

// -- BasicBlock 

BasicBlock IRBuilder::getCurrentBasicBlock() {
  return {*this, Builder->GetInsertBlock()};
}

// -- Value 

Value IRBuilder::mkInt(int val) { return {*this, Builder->getInt32(val)}; }
  
// -- AllocaValue
  
AllocaValue IRBuilder::mkAlloca(Value v) { return AllocaValue(v); }

// -- Function

Function IRBuilder::mkFunctionImpl(
    std::string name, std::vector<Type> ret_type,
    std::vector<std::pair<Type, std::string>> args) {

  auto retTy = ret_type.empty() ? Builder->getVoidTy() : ret_type[0].get();

  std::vector<llvm::Type*> argsTy;
  argsTy.reserve(args.size());
  for (auto arg : args) {
    argsTy.push_back(arg.first.get());
  }

  llvm::FunctionType* funcType = llvm::FunctionType::get(retTy, argsTy, false);
  llvm::Function* func         = llvm::Function::Create(
      funcType, llvm::Function::ExternalLinkage, name, &Module);

  int count = 0;
  for (auto& arg : func->args()) {
    arg.setName(args[count++].second);
  }
  return Function{*this, func};
}

Function IRBuilder::mkVoidFunction(
    std::string name, std::vector<std::pair<Type, std::string>> args) {
  return mkFunctionImpl(std::move(name), {}, std::move(args));
}
Function IRBuilder::mkFunction(
    std::string name, Type ret_type,
    std::vector<std::pair<Type, std::string>> args) {
  return mkFunctionImpl(std::move(name), {ret_type}, std::move(args));
}
  
// -- CallInst

CallInst IRBuilder::mkCall(Function f, std::vector<Value> args) {
  assert(args.size() == f.n_args() && "Argument count mismatch");
  std::vector<llvm::Value*> arguments;
  arguments.reserve(args.size());
  for (auto& arg : args) {
    arguments.push_back(arg.get());
  }
  return {*this, Builder->CreateCall(f.get(), arguments)};
}

// -- BranchInst

BranchInst IRBuilder::mkBranch(BasicBlock bb) {
  return {*this, Builder->CreateBr(bb.get())};
}

// -- PhiNode

PhiNode IRBuilder::mkPhiNode(
    Type type, std::vector<std::pair<Value, BasicBlock>> edge_list) {
  llvm::PHINode* phi = Builder->CreatePHI(type.get(), edge_list.size());
  for (auto& edge : edge_list) {
    phi->addIncoming(edge.first.get(), edge.second.get());
  }
  return {*this, phi};
}
  
// -- Ret

void IRBuilder::mkRet(Value val) { Builder->CreateRet(val.get()); }
void IRBuilder::mkRetVoid() { Builder->CreateRetVoid(); }

// -- Loops

template <class F>
Value IRBuilder::mkLoop(Value begin, Value end, Value step, F body) {
  auto bb = getCurrentBasicBlock();

  // create basic blocks for a loop structure
  auto condBB  = bb.getParent().mkBasicBlock("condBB");
  auto bodyBB  = bb.getParent().mkBasicBlock("boduyBB");
  auto afterBB = bb.getParent().mkBasicBlock("afterBB");

  // initialize induction variable to init value
  auto iv = mkAlloca(begin);
  mkBranch(condBB);

  // verify condition
  condBB.set();
  auto cond = iv.load() < end;
  cond.mkIfThenElse(bodyBB, afterBB);

  // place loop body
  bodyBB.set();
  body(iv.load());
  iv += step;
  mkBranch(condBB);

  // finalize loop
  afterBB.set();
  return iv.load();
}

template <class F>
std::vector<Value> IRBuilder::mkNdLoop(
    std::vector<std::tuple<Value, Value, Value>> trip_count,
    F body) {
  assert(!trip_count.empty());

  auto const iv_count = trip_count.size();

  // for perf reasons, we'll be iterating from the last to the first index
  // so invert, the array to preserve logical loop structure
  std::reverse(trip_count.begin(), trip_count.end());

  // current, and ending induction variable lists
  std::vector<Value> ivs, end_ivs;

  // basic block list for each loop
  std::vector<BasicBlock> end_bbs;

  // reserve storage
  ivs.reserve(iv_count);
  end_ivs.reserve(iv_count);
  end_bbs.reserve(iv_count);

  // recursive ND loop implementation
  std::function<void()> impl = [&, this]() {

    // if we processed all indices, just emplace loop body, and return
    if (trip_count.empty()) {

      assert(ivs.size() == iv_count);

      // since ivs are stored in reverse order, so reverse the vector :)
      std::reverse(ivs.begin(), ivs.end());

      // emplace function body
      body(ivs);

      // done!
      return;
    }

    // otherwise, process the following index
    
    auto ivc = trip_count.back();
    trip_count .pop_back();

    using std::get;
    // emit 1D loop, which recursively calls this function
    auto end_iv = mkLoop(get<0>(ivc), get<1>(ivc), get<2>(ivc), [&](Value iv) {

      //store currend iv
      ivs.push_back(iv);

      // call itself
      impl();
    });

    end_ivs.push_back(end_iv);
    end_bbs.push_back(getCurrentBasicBlock());
  };

  // generate the loop
  impl();
  std::reverse(end_ivs.begin(), end_ivs.end());
  end_bbs.back().set();

  assert(ivs.size() == iv_count);
  assert(end_ivs.size() == ivs.size());
  assert(end_bbs.size() == ivs.size());
  return end_ivs;
}


} // namespace LLVMCodegen
