#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <unordered_set>
#include <vector>
#include <string>
#include <iostream>

namespace LLVMIRBuilder {

#if 1
#define ALIAS_OTHER_LLVM_IRBUILDER
#endif

struct Type;
struct BasicBlock;
struct Boolean;
struct Value;
struct Alloca;
struct Function;
struct CallInst;
struct PhiNode;
struct BranchInst;

struct IRBuilder {
private:
  using LLVMIRBuilder = llvm::IRBuilder<>;

  llvm::Module& Module;
  LLVMIRBuilder* Builder;
  bool LLVMIRBuilderAliased;

public:
  // ctor to alias existing llvm::IRBuilder
  explicit IRBuilder(llvm::Module& module, LLVMIRBuilder& builder)
      : Module(module), Builder(&builder), LLVMIRBuilderAliased(true) {}

  // ctor to create brand new llvm::IRBuilder
  explicit IRBuilder(llvm::Module& module) : Module(module),
                                             LLVMIRBuilderAliased(false) {
    Builder = new LLVMIRBuilder(getContext());
  }

  // getter for llvm::IRBuilder
  LLVMIRBuilder* operator->() { return Builder; }

  // getter for current context
  llvm::LLVMContext& getContext() { return Module.getContext(); }

  // getter for current module
  llvm::Module& getModule() { return Module; }

  void dump() const { Module.dump(); }
  bool verifyModule() { return llvm::verifyModule(Module); }

  // dtor: if Builder is not thei alias to existing llvm::IRBuilder, 
  //       delete it then
  ~IRBuilder() {
    if (!LLVMIRBuilderAliased) {
      delete Builder;
    }
  }

  // -- method declarations

  // -- Type 
  // --------------------------------------------------------------------------
  
  /// Factory methods for int32 type
  /// @return   int32 type
  Type mkIntTy();

  /// Factory methods for float32 type
  /// @return   float32 type
  Type mkFloatTy();
  
  /// Factory methods for float64 type
  /// @return   float64 type
  Type mkDoubleTy();
  
  // -- BasicBlock
  // --------------------------------------------------------------------------
  
  /// Obtain current basic block
  /// @return  current basic blick
  BasicBlock getCurrentBasicBlock();

  /// Factory method for a new basic block
  /// @name    Name of the new basic block
  /// @return  New basic block
  BasicBlock mkBasicBlock(std::string name);

  // -- Value
  // --------------------------------------------------------------------------

  /// Manufacture IR representation for integer value
  /// @value   input integer value
  /// @return  IR representation for integer value
  Value mkInt(int value);

  /// Manufacture IR representation for APInt value
  /// @value   Input APInt  value
  /// @return  wrapper AIPInt value
  Value mkAPInt(llvm::APInt value);

  /// Manufacture IR representation for float32 value
  /// @value   input float32 value
  /// @return  IR representation for float32 value
  Value mkFloat(float value);

  /// Manufacture IR representation for float64 value
  /// @value   input float64 value
  /// @return  IR representation for float64 value
  Value mkDouble(double value);

  /// Manufacture IR representation for desired type
  /// @type    IR representation of input type
  /// @value   Input value (will be cast to input type)
  /// @return  IR representation for float64 value
  template<class T>
  Value mkValue(Type type, T value);

  /// Wrap llvm::Value 
  /// @input   llvm::Value 
  /// @return  wrapped llvm::value
  Value wrapLLVMValue(llvm::Value* input);

  // -- Alloca
  // --------------------------------------------------------------------------

  /// Manufacture alloca from Value
  /// @value    Input value
  /// @return   Alloca holding the value
  Alloca mkAlloca(Value value);

  // -- Function
  // --------------------------------------------------------------------------

private:
  // implementaiton detail for public mkfunction
  // can construct with void and non-void functions
  Function mkFunctionImpl(std::string name,
                          std::vector<Type> ret_type,
                          std::vector<std::pair<Type, std::string>> args);

public:
  /// Manufacture void function declartion
  /// @name    function name
  /// @parms   list of function parameters, each is a pair (type, name)
  /// @return  IR of void function declaration
  Function mkVoidFunction(std::string name,
                          std::vector<std::pair<Type, std::string>> parms);

  /// Manufacture non-void function declartion
  /// @name      function name
  /// @ret_type  function return type
  /// @parms     list of function parameters, each is a pair (type, name)
  /// @return    IR of non-void function declaration
  Function mkFunction(std::string name, Type ret_type,
                      std::vector<std::pair<Type, std::string>> parms);

  // -- CallInst
  // --------------------------------------------------------------------------

  /// Create call instruction
  /// @fun      function
  /// @args     list of argumennts
  /// @return   IR for function call 
  CallInst mkCall(Function fun, std::vector<Value> args);
  
  // -- BranchInst
  // --------------------------------------------------------------------------
 
  /// Make branch instruction 
  /// @basic_block   basic block to branch
  /// @return        IR for branch instruction
  BranchInst mkBranch(BasicBlock basic_block);
  
  // -- PhiNode
  // --------------------------------------------------------------------------

  /// Create Phi node
  /// @type        type of a phi node
  /// @edge_list   List of incoming edges, each is pair of (Value,BasicBlock)
  ///              more edges can be later add with += {Value,BB} operator
  /// @return      IR for phi instruction
  PhiNode mkPhiNode(Type type,
                    std::vector<std::pair<Value, BasicBlock>> edge_list);
  
  // -- Misc
  // --------------------------------------------------------------------------

  /// Create void return instruction
  void mkRetVoid();

  /// Create non-return instruction 
  /// @value   value to return
  void mkRet(Value value);

  // -- Loops
  // --------------------------------------------------------------------------
  
 
  // Manufactor 1D loop with exit block probided
  template <class F>
  void mkLoopWithExitBlock(Value begin, Value end, Value step,
                          BasicBlock exitBB, F body);
  
  /// Manfacture ND while-loop: 
  /// @trip_count  List of trip_count tuples
  ///                  field 0: begin
  ///                  field 1: end
  ///                  field 2: step
  /// @body      function object that emits body IR
  ///              the function must have the following signature
  ///              (std::vector<Value>) where vector has same order
  ///                                   of IVs as the trip count
  /// @return    Vector of induction variable at loop end
  template <class F>
  void mkNdLoop(std::vector<std::tuple<Value, Value, Value>> trip_count,
                F body);

  /// Manfacture 1D while-loop: 
  ///       for (iv = begin; iv < end; iv += step) { body(iv) };
  /// @begin     Begin value
  /// @end       End value
  /// @step      Step to increment induction variable
  /// @body      function object that emits body IR
  ///              the function must have the following signature
  ///              (BasicBlock, Value) where
  ///                BasicBlock - loop besic block (you may espace it if needed)
  ///                Value      - value of inducation variable
  /// @return    Value of induction variable at loop end
  template <class F>
  void mkLoop(Value begin, Value end, Value step, F body);


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
      assert(0 && "Internal error");
    }
  }

  Kind getTypeKind() const { return TypeKind; }

  llvm::Type* get() const {
    switch (TypeKind) {
    case int32:
      return Builder->getInt32Ty();
    case float32:
      return Builder->getFloatTy();
    case float64:
      return Builder->getDoubleTy();
    default:
      assert(0);
    }
  }

  size_t size() const {
    switch (TypeKind) {
    case int32:
    case float32:
      return 4;
    case float64:
      return 8;
    default:
      assert(0);
    }
  }

  bool operator==(Type const& other) { return TypeKind == other.TypeKind; }
}; // struct Type
  
//--------------------- Basic Block -------------------------------------------

// -- Function forward declaration
//    required since BasicBlock can return its parent function scope
struct BasicBlock : IRBuilderRef {
  llvm::BasicBlock* BB;
  BasicBlock(IRBuilder& builder, llvm::BasicBlock* bb) : IRBuilderRef(builder),
  BB(bb) {}
  llvm::BasicBlock* get() const { return BB; }
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

  llvm::Value* get() const { return V; }

  template <class Then, class Else>
  void mkIfThenElse(BasicBlock thenBB, BasicBlock elseBB,
                    Then thenF, Else elseF);
  template <class Then, class Else>
  void ifThenElse(Then thenF, Else elseF);

  template <class Then>
  void ifThen(Then thenF);

  // -- declaration
  Value select(Value thenV, Value elseV);

  Boolean operator&(Boolean const& R) {
    return {Builder, Builder->CreateAnd(this->get(), R.get(), "and")};
    }
    Boolean operator|(Boolean const& R) {
      return {Builder, Builder->CreateOr(this->get(), R.get(), "or")};
    }
    Boolean operator^(Boolean const& R) {
      return {Builder, Builder->CreateXor(this->get(), R.get(), "xor")};
    }
    Boolean operator!() {
      return {Builder, Builder->CreateNot(this->get(), "not")};
    }
}; // struct Boolean

// -----------------------  Value -------------------------------------------

struct Value : IRBuilderRef {

  enum InitialValue {
    ZERO,
    MIN,
    MAX
  };
  llvm::Value* V;

  Value(IRBuilder& ref, llvm::Value* value) : IRBuilderRef(ref), V(value) {}
  Type getType() { return {Builder, V->getType()}; }

  llvm::Value* get() const { return V; }

  Value operator*(Value R) {
    assert(getType() == R.getType());
    switch(getType().getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateMul(this->get(), R.get(), "imul")};
      case Type::float32:
      case Type::float64:
        return {Builder, Builder->CreateFMul(this->get(), R.get(), "fmul")};
      default:
        assert(0);
    };
  }

  Value operator+(Value R) {
    assert(getType() == R.getType());
    switch(getType().getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateAdd(this->get(), R.get(), "iadd")};
      case Type::float32:
      case Type::float64:
        return {Builder, Builder->CreateFAdd(this->get(), R.get(), "fadd")};
      default:
        assert(0);
    };
  }
  Value operator/(Value R) {
    assert(getType() == R.getType());
    switch(getType().getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateSDiv(this->get(), R.get(), "idiv")};
      case Type::float32:
      case Type::float64:
        return {Builder, Builder->CreateFDiv(this->get(), R.get(), "fdiv")};
      default:
        assert(0);
    };
  }

  Value operator-(Value R) {
    assert(getType() == R.getType());
    switch(getType().getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateSub(this->get(), R.get(), "isub")};
      case Type::float32:
      case Type::float64:
        return {Builder, Builder->CreateFSub(this->get(), R.get(), "fsub")};
      default:
        assert(0);
    };
  }


  Boolean operator<(Value R) {
    assert(getType() == R.getType());
    switch(getType().getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateICmpSLT(this->get(), R.get(), "ilt")};
      case Type::float32:
      case Type::float64:
        return {Builder, Builder->CreateFCmpULT(this->get(), R.get(), "flt")};
      default:
        assert(0);
    };
  }
  Boolean operator>(Value R) {
    assert(getType() == R.getType());
    switch(getType().getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateICmpSGT(this->get(), R.get(), "igt")};
      case Type::float32:
      case Type::float64:
        return {Builder, Builder->CreateFCmpUGT(this->get(), R.get(), "fgt")};
      default:
        assert(0);
    };
  }
  Boolean operator>=(Value R) {
    assert(getType() == R.getType());
    switch(getType().getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateICmpSGE(this->get(), R.get(), "ige")};
      case Type::float32:
      case Type::float64:
        return {Builder, Builder->CreateFCmpUGE(this->get(), R.get(), "fge")};
      default:
        assert(0);
    };
  }
  Boolean operator<=(Value R) {
    assert(getType() == R.getType());
    switch(getType().getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateICmpSLE(this->get(), R.get(), "ile")};
      case Type::float32:
      case Type::float64:
        return {Builder, Builder->CreateFCmpULE(this->get(), R.get(), "fle")};
      default:
        assert(0);
    };
  }
  Boolean operator!=(Value R) {
    assert(getType() == R.getType());
    switch(getType().getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateICmpNE(this->get(), R.get(), "ine")};
      case Type::float32:
      case Type::float64:
        return {Builder, Builder->CreateFCmpUNE(this->get(), R.get(), "fne")};
      default:
        assert(0);
    };
  }
  Boolean operator==(Value R) {
    assert(getType() == R.getType());
    switch(getType().getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateICmpEQ(this->get(), R.get(), "ieq")};
      case Type::float32:
      case Type::float64:
        return {Builder, Builder->CreateFCmpUEQ(this->get(), R.get(), "feq")};
      default:
        assert(0);
    };
  }
  Value operator-() {
    switch(getType().getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateNeg(this->get(), "ineg")};
      case Type::float32:
      case Type::float64:
        return {Builder, Builder->CreateFNeg(this->get(), "fneg")};
      default:
        assert(0);
    };
  }

  Value castTo(Type type) {
    switch (this->getType().getTypeKind()) {
    case Type::int32:
      switch (type.getTypeKind()) {
      case Type::int32: 
        return *this;
      case Type::float32:
        return {Builder, Builder->CreateSIToFP(this->get(),
                                               Builder.mkFloatTy().get())};
      case Type::float64:
        return {Builder, Builder->CreateSIToFP(this->get(),
                                               Builder.mkDoubleTy().get())};
      };
    case Type::float32:
      switch (type.getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateFPToSI(this->get(),
                                               Builder.mkIntTy().get())};
      case Type::float32:
        return *this;
      case Type::float64:
        return {Builder, Builder->CreateFPCast(this->get(),
                                               Builder.mkDoubleTy().get())};
      };
    case Type::float64: 
      switch (type.getTypeKind()) {
      case Type::int32:
        return {Builder, Builder->CreateFPToSI(this->get(),
                                               Builder.mkIntTy().get())};
      case Type::float32:
        return {Builder, Builder->CreateFPCast(this->get(),
                                               Builder.mkFloatTy().get())};
      case Type::float64:
        return *this;
      };
    }
    assert(0);
  }
}; // struct Value
Value Boolean::select(Value thenV, Value elseV) {
  return {Builder, Builder->CreateSelect(V, thenV.get(), elseV.get())};
}

// ----------------------------- Alloca ---------------------------------------

struct Alloca : IRBuilderRef {
  llvm::Value* V;

  explicit Alloca(Type type) : IRBuilderRef(type.Builder) {
    V = Builder->CreateAlloca(type.get());
  }

  Value load() { return {Builder, Builder->CreateLoad(V)}; }
  void store(Value v) { Builder->CreateStore(v.get(), V); }
}; // struct Alloca

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

  llvm::Function* get() const { return F; }
  std::string const& getErrorString() { return ErrorString; }
  std::string getName() const { return std::string(F->getName().data()); }

  Value arg(size_t num) {
    assert(num < Args.size());
    return Value{Builder, Args[num]};
  }

  BasicBlock mkBasicBlock(std::string name) {
    return {Builder, llvm::BasicBlock::Create(Builder.getContext(), name, F)};
  }

  BasicBlock getEntryBasicBlock() {
    return {Builder, &F->getEntryBlock()};
  }
}; // struct Function
Function BasicBlock::getParent() { return {Builder, BB->getParent()}; }

// -------------------------- CallInst ----------------------------------------

struct CallInst : IRBuilderRef {
  llvm::CallInst* Inst;
  CallInst(IRBuilder& ref, llvm::CallInst* inst) : IRBuilderRef(ref),
                                                   Inst(inst) {}

  explicit operator Value() { return {Builder, Inst}; }
  llvm::CallInst* get() const { return Inst; }
}; // struct CallInst

// -------------------------- BranchInst ------------------------------------

struct BranchInst : IRBuilderRef {
  llvm::BranchInst* Inst;
  BranchInst(IRBuilder& ref, llvm::BranchInst* inst) : IRBuilderRef(ref),
                                                       Inst(inst) {}
  explicit operator Value() { return {Builder, Inst}; }
  llvm::BranchInst* get() const { return Inst; }
};

// ------------------------------- PhiNode ------------------------------------

struct PhiNode : IRBuilderRef {
  llvm::PHINode* V;

  PhiNode(IRBuilder& ref, llvm::PHINode* v) : IRBuilderRef(ref), V(v) {}
  llvm::PHINode* get() const { return V; }

  explicit operator Value() { return {Builder, V}; }

  PhiNode& operator+=(std::pair<Value, BasicBlock> rhs) {
    V->addIncoming(rhs.first.get(), rhs.second.get());
    return *this;
  }
};

// --------------------------------------------------------------------------
// -- Boolean method definitions
// --------------------------------------------------------------------------

template <class Then, class Else>
void Boolean::mkIfThenElse(BasicBlock thenBB, BasicBlock elseBB,
                           Then thenF, Else elseF) {
  Builder->CreateCondBr(V, thenBB.get(), elseBB.get());
  thenBB.set();
  thenF();
  elseBB.set();
  elseF();
}

template <class Then, class Else>
void Boolean::ifThenElse(Then thenF, Else elseF) {
  auto thenBB = Builder.mkBasicBlock("then");
  auto elseBB = Builder.mkBasicBlock("else");
  auto contBB = Builder.mkBasicBlock("cont");
  mkIfThenElse(thenBB, elseBB,
               // then
               [&contBB, &thenF, this]() {
                 thenF();
                 Builder.mkBranch(contBB);
               },
               // else
               [&contBB, &thenF, this]() {
                 thenF();
                 Builder.mkBranch(contBB);
               });
  contBB.set();
}

template <class Then>
void Boolean::ifThen(Then thenF) {
  auto thenBB = Builder.mkBasicBlock("then");
  auto contBB = Builder.mkBasicBlock("cont");
  mkIfThenElse(thenBB, thenBB,
               // then
               [&contBB, &thenF, this]() {
                 thenF();
                 Builder.mkBranch(contBB);
               },
               []() {});
  contBB.set();
}

// --------------------------------------------------------------------------
// -- IRBuilder method definitions
// --------------------------------------------------------------------------

// -- RetTy: Type
// --------------------------------------------------------------------------

Type IRBuilder::mkIntTy() {
      return {*this, Type::int32}; }
Type IRBuilder::mkFloatTy() {
      return {*this, Type::float32}; }
Type IRBuilder::mkDoubleTy() {
      return {*this, Type::float64}; }

// -- RetTy: BasicBlock
// --------------------------------------------------------------------------

BasicBlock IRBuilder::getCurrentBasicBlock() {
      return {*this, Builder->GetInsertBlock()};
}
BasicBlock IRBuilder::mkBasicBlock(std::string name) {
      auto bb = getCurrentBasicBlock();
      auto f  = bb.getParent();
      return f.mkBasicBlock(name);
}

// -- RetTy: Value 
// --------------------------------------------------------------------------

Value IRBuilder::mkInt(int val) {
      return {*this, Builder->getInt32(val)};
}
Value IRBuilder::mkAPInt(llvm::APInt val) {
      return {*this, llvm::Constant::getIntegerValue(Builder->getInt32Ty(), val)};
}
Value IRBuilder::mkFloat(float val) {
      return {*this, llvm::ConstantFP::get(llvm::Type::getFloatTy(getContext()), val)};
}
Value IRBuilder::mkDouble(double val) {
      return {*this, llvm::ConstantFP::get(llvm::Type::getDoubleTy(getContext()), val)};
}
Value IRBuilder::wrapLLVMValue(llvm::Value* val) {
      return {*this, val}; }
template <class T>
Value IRBuilder::mkValue(Type type, T value) {
      switch (type.getTypeKind()) {
      case Type::int32:
        return mkInt(static_cast<int>(value));
      case Type::float32:
        return mkFloat(static_cast<float>(value));
      case Type::float64:
        return mkDouble(static_cast<double>(value));
  };
  assert(0);
}
template <>
Value IRBuilder::mkValue<Value::InitialValue>(Type type,
                                              Value::InitialValue what) {
  int bit_width = 32; //type.get()->getBitWidth()))};
  switch (what) {
  case Value::ZERO:
    switch (type.getTypeKind()) {
    case Type::int32:
      return mkInt(0);
    case Type::float32:
      return mkFloat(llvm::APFloat::getZero(llvm::APFloat::IEEEsingle).convertToFloat());
    case Type::float64:
      return mkDouble(llvm::APFloat::getZero(llvm::APFloat::IEEEdouble).convertToDouble());
    };
  case Value::MIN: 
    switch (type.getTypeKind()) {
      case Type::int32:
        return mkAPInt(llvm::APInt::getSignedMinValue(bit_width));
      case Type::float32:
        return mkFloat(llvm::APFloat::getSmallest(llvm::APFloat::IEEEsingle, true).convertToFloat());
      case Type::float64:
        return mkDouble(llvm::APFloat::getSmallest(llvm::APFloat::IEEEdouble, true).convertToDouble());
    };
  case Value::MAX: 
    switch (type.getTypeKind()) {
      case Type::int32:
        return mkAPInt(llvm::APInt::getSignedMaxValue(bit_width));
      case Type::float32:
        return mkFloat(llvm::APFloat::getLargest(llvm::APFloat::IEEEsingle).convertToFloat());
      case Type::float64:
        return mkDouble(llvm::APFloat::getLargest(llvm::APFloat::IEEEdouble).convertToDouble());
    };
  };
  assert(0);
}

// -- RetTy: Alloca
// --------------------------------------------------------------------------

Alloca IRBuilder::mkAlloca(Value value) { 
  auto currBB = getCurrentBasicBlock();
  auto entryBB = currBB.getParent().getEntryBasicBlock();
  // if first instruction found, set this to be insertionp point
  if (auto firstI = entryBB.get()->getFirstNonPHIOrDbg()) {
    Builder->SetInsertPoint(firstI);
  } else {
    // ohterwise set entryBB first since this is the first instruction
    entryBB.set();
  }
  auto alloca =  Alloca(value.getType()); 
  currBB.set();
  alloca.store(value);
  return alloca;
}

// -- RetTy: Function
// --------------------------------------------------------------------------

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
  
// -- RetTy: CallInst
// --------------------------------------------------------------------------

CallInst IRBuilder::mkCall(Function f, std::vector<Value> args) {
  assert(args.size() == f.n_args() && "Argument count mismatch");
  std::vector<llvm::Value*> arguments;
  arguments.reserve(args.size());
  for (auto& arg : args) {
    arguments.push_back(arg.get());
  }
  return {*this, Builder->CreateCall(f.get(), arguments)};
}

// -- RetTy: BranchInst
// --------------------------------------------------------------------------

BranchInst IRBuilder::mkBranch(BasicBlock bb) {
  return {*this, Builder->CreateBr(bb.get())};
}

// -- RetTy: PhiNode
// --------------------------------------------------------------------------

PhiNode IRBuilder::mkPhiNode(
    Type type, std::vector<std::pair<Value, BasicBlock>> edge_list) {
  llvm::PHINode* phi = Builder->CreatePHI(type.get(), edge_list.size());
  for (auto& edge : edge_list) {
    phi->addIncoming(edge.first.get(), edge.second.get());
  }
  return {*this, phi};
}
  
// -- Misc
// --------------------------------------------------------------------------

void IRBuilder::mkRet(Value val) { Builder->CreateRet(val.get()); }
void IRBuilder::mkRetVoid() { Builder->CreateRetVoid(); }
Value min(Value a, Value b) { return (a < b).select(a, b); }
Value max(Value a, Value b) { return (a > b).select(a, b); }

// -- Loops

template <class F>
void IRBuilder::mkLoopWithExitBlock(Value begin, Value end, Value step,
                                    BasicBlock exitBB, F body) {
  auto entryBB = getCurrentBasicBlock();
  auto headerBB = entryBB.getParent().mkBasicBlock("header");
  auto loopBB   = entryBB.getParent().mkBasicBlock("body");
  auto latchBB  = entryBB.getParent().mkBasicBlock("latch");
  auto footerBB = entryBB.getParent().mkBasicBlock("footer");


  ////////////
  // entry:
  //     br header
  // header:
  //     phi = [begin, header], [iv, latch]
  //     cond =  phi < end
  //
  //     if (cond) {
  //        br loop
  //     } else {
  //        br footer
  //     }
  // loop:
  //     body(phi)
  //     br latch
  //
  // latch:
  //     iv = phi + step
  //     br header
  //
  // footer:
  //     br exitBB
  /////////////
  
  mkBranch(headerBB);

  headerBB.set();
  auto phi = mkPhiNode(begin.getType(), {{begin, entryBB}});
  Value iv = static_cast<Value>(phi);
  auto cond = iv < end;

  cond.mkIfThenElse(loopBB, footerBB, []() {}, []() {});

  loopBB.set();
  body(iv);
  mkBranch(latchBB);

  latchBB.set();
  iv = iv + step;
  phi += {iv, latchBB};
  mkBranch(headerBB);

  footerBB.set();
  mkBranch(exitBB);
}
// --------------------------------------------------------------------------

template <class F>
void IRBuilder::mkNdLoop(
    std::vector<std::tuple<Value, Value, Value>> trip_count, F body) {
  assert(!trip_count.empty());

  auto const iv_count = trip_count.size();

  // current, and ending induction variable lists
  std::vector<Value> ivs;

  // reserve storage
  ivs.reserve(iv_count);

  // recursive ND loop implementation
  std::function<void()> impl =
      [&ivs, &iv_count, &trip_count, &body, &impl, this]() {

        // if we processed all indices, just emplace loop body, and return
        if (trip_count.empty()) {

          assert(ivs.size() == iv_count);

          // we want ivs to have same order as trip_count
          // since we process trip_count from the end,
          // we need to reverse ivs vector before passing it to the loop body
          std::reverse(ivs.begin(), ivs.end());

          // emplace function body
          body(ivs);

          // done!
          return;
        }

        // otherwise, process the following index

        auto ivc = trip_count.back();
        trip_count.pop_back();

        using std::get;
        // emit 1D loop, which recursively calls this function
        auto exitBB = mkBasicBlock("exit");
        mkLoopWithExitBlock(get<0>(ivc), get<1>(ivc), get<2>(ivc),
                            exitBB,
                            [&ivs, &impl](Value iv) {

                              // store current iv
                              ivs.push_back(iv);

                              // call self
                              impl();
                            });
        // set exit point for 1D loop
        exitBB.set();
      };

  // generate the loop
  impl();
}

template <class F>
void IRBuilder::mkLoop(Value begin, Value end, Value step, F body) {
  mkNdLoop({std::make_tuple(begin, end, step)},
           [&body](std::vector<Value> iv) { return body(iv[0]); });
}


} // namespace LLVMIRBuilder
