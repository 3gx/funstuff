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


  //----------------------- Type ---------------------------------------------

  struct Type {
    enum Kind { int32, float32, float64 };

    IRBuilder &Builder;
    Kind TypeKind;

    Type(IRBuilder &builder, Kind type_kind)
        : Builder(builder), TypeKind(type_kind) {}

    operator llvm::Type *() const {
      switch (TypeKind) {
      case int32:
        return Builder.getInt32Ty();
      case float32:
        return Builder.getFloatTy();
      case float64:
        return Builder.getDoubleTy();
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
    }
  };

  Type mkInt() const { return Type(Builder, Type::int32); }
  Type mkFloat() const { return Type(Builder, Type::float32); }
  Type mkDouble() const { return Type(Builder, Type::float64); }


  // --------------------- Global Variable -----------------------------------
  struct GlobalVar {
    IRBuilder &Builder;
    llvm::GlobalVariable &Var;
    GlobalVar(IRBuilder &builder, llvm::GlobalVariable &var)
        : Builder(builder), Var(var) {}
  };

  GlobalVar mkGlobalVar(std::string name, Type type) {
    M.getOrInsertGlobal(name, type);
    llvm::GlobalVariable *gvar = M.getNamedGlobal(name);
    gvar->setLinkage(llvm::GlobalValue::CommonLinkage);
    gvar->setAlignment(type.size());
    return GlobalVar{Builder, *gvar};
  }

  //---------------------- Function -------------------------------------------
  
  struct Function {
    llvm::Function &F;
    Function(llvm::Function &f) : F(f) {}
    void verify() const { llvm::verifyFunction(F); }

    operator llvm::Function *() { return &F; }
  };

  Function mkFunction(std::string name, Type ret_type,
                      std::vector<Type> arg_type = {}) {
    llvm::FunctionType *funcType = llvm::FunctionType::get(ret_type, false);
    llvm::Function *func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, name, &M);
    return Function{*func};
  }


  //------------------- Basic Block -------------------------------------------

  struct BasicBlock {
    IRBuilder &Builder;
    llvm::BasicBlock &BB;
    BasicBlock(IRBuilder &builder, llvm::BasicBlock &bb)
        : Builder(builder), BB(bb) {}
    operator llvm::BasicBlock *() { return &BB; }
    void set() { Builder.SetInsertPoint(&BB); }
  };

  BasicBlock mkBasicBlock(Function &func, std::string name) {
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(Context, name, func);
    return BasicBlock{Builder, *bb};
  }


};

static llvm::LLVMContext &ContextRef = llvm::getGlobalContext();
static llvm::Module *ModuleOb = new llvm::Module("my compiler", ContextRef);

int main(int argc, char *argv[]) {
  static llvm::IRBuilder<> BuilderObj(ContextRef);
  LLVMCodeGen cg(ContextRef, *ModuleOb, BuilderObj);

  auto gvar = cg.mkGlobalVar("x", cg.mkFloat());

  auto f = cg.mkFunction("foo", cg.mkInt());
  auto entry = cg.mkBasicBlock(f, "entry");
  entry.set();
  f.verify();

  cg.dump();
  return 0;
}
