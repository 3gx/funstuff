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
  using LLVMIRBuilder = llvm::IRBuilder<>;

  llvm::Module& Module;
  std::unique_ptr<LLVMIRBuilder> Builder;

  explicit IRBuilder(llvm::Module& module) : Module(module) {
    Builder.reset(new LLVMIRBuilder(getContext()));
  }

  llvm::LLVMContext& getContext() {
    return Module.getContext();
  }

  llvm::Module& getModule() { return Module; }

  LLVMIRBuilder* operator->() { return Builder.get(); }
}; // struct IRBuilder

struct IRBuilderRef {
  IRBuilder& Builder;
  IRBuilderRef(IRBuilder& builder) : Builder(builder){};
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
  llvm::Value* V;

  explicit AllocaValue(Value v) : IRBuilderRef(v.Builder) {
    V = Builder->CreateAlloca(v.get()->getType());
    store(v);
  }

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

// -------------------------- BranchInst ------------------------------------

struct BranchInst : IRBuilderRef {
  llvm::BranchInst* Inst;
  BranchInst(IRBuilder& ref, llvm::BranchInst* inst) : IRBuilderRef(ref),
                                                       Inst(inst) {}
  explicit operator Value() { return {Builder, Inst}; }
  llvm::BranchInst* get() { return Inst; }
};

} // namespace LLVMCodegen
