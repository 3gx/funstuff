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
  
  // -----------------------  Value -------------------------------------------
  struct Value {
    LLVMCodeGen &CG;
    llvm::Value &V;

    Value(LLVMCodeGen &cg, llvm::Value &value) : CG(cg), V(value) {}

    operator llvm::Value *() { return &V; }

    Value operator*(Value R) const {
      return Value{CG, *CG.Builder.CreateMul(&V, R, "mul")};
    }
  };

  //----------------------- Type ---------------------------------------------

  struct Type {
    enum Kind { int32, float32, float64 };

    LLVMCodeGen const &CG;
    Kind TypeKind;

    Type(LLVMCodeGen const &cg, Kind type_kind) : CG(cg), TypeKind(type_kind) {}

    operator llvm::Type *() const {
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

    size_t size() const {
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
    M.getOrInsertGlobal(name, type);
    llvm::GlobalVariable *gvar = M.getNamedGlobal(name);
    gvar->setLinkage(llvm::GlobalValue::CommonLinkage);
    gvar->setAlignment(type.size());
    return GlobalVar{*this, *gvar};
  }
  
  //------------------- Basic Block -------------------------------------------

  struct BasicBlock {
    LLVMCodeGen &CG;
    llvm::BasicBlock &BB;
    BasicBlock(LLVMCodeGen &cg, llvm::BasicBlock &bb) : CG(cg), BB(bb) {}
    operator llvm::BasicBlock *() { return &BB; }
    void set() { CG.Builder.SetInsertPoint(&BB); }

  };


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

    operator llvm::Function *() { return &F; }

    Value arg(size_t num) const {
      assert(num < Args.size());
      return Value{CG, *Args[num]};
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
      args_type.push_back(arg.first);
    }
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(ret_type, args_type, false);
    llvm::Function *func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, name, &M);
    int count = 0;
    for (auto &arg : func->args()) {
      arg.setName(args[count++].second);
    }
    return Function{*this, *func};
  }


  // ------------- type & vals
 
  Type mkInt() { return Type(*this, Type::int32); }
  Value mkInt(int val) { return {*this, *Builder.getInt32(val)}; }
  Type mkFloat() { return Type(*this, Type::float32); }
  Type mkDouble() { return Type(*this, Type::float64); }

  //-------------------- Ops

  void mkRet(Value val) { Builder.CreateRet(val); }
};

static llvm::LLVMContext &ContextRef = llvm::getGlobalContext();
static llvm::Module *ModuleOb = new llvm::Module("my compiler", ContextRef);

int main(int argc, char *argv[]) {
  static llvm::IRBuilder<> BuilderObj(ContextRef);
  LLVMCodeGen cg(ContextRef, *ModuleOb, BuilderObj);

  auto gvar = cg.mkGlobalVar("x", cg.mkFloat());

  auto f = cg.mkFunction("foo", cg.mkInt(),
                         {{cg.mkInt(), "a"}, {cg.mkFloat(), "b"}});
  auto entry = f.mkBasicBlock("entry");
  entry.set();

  auto constant = cg.mkInt(16);
  auto val      = constant * f.arg(0);
  cg.mkRet(val);

  f.verify();

  cg.dump();
  return 0;
}
