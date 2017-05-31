#ifdef NDEBUG
#undef NDEBUG
#endif

#include "LLVMIRBuilder.hpp"

static llvm::LLVMContext &ContextRef = llvm::getGlobalContext();
static llvm::Module *ModuleOb = new llvm::Module("my compiler", ContextRef);

namespace IR = LLVMIRBuilder;

int main(int argc, char *argv[]) {
//  static llvm::IRBuilder<> BuilderObj(ContextRef);
//  LLVMCodeGen cg(ContextRef, *ModuleOb, BuilderObj);

  IR::IRBuilder cg(*ModuleOb);

#if 0
  auto gvar = cg.mkGlobalVar("x", cg.mkFloatTy());
#endif

  auto f = cg.mkFunction("foo", cg.mkFloatTy(),
                         {{cg.mkIntTy(), "a"}, {cg.mkIntTy(), "b"}});

  auto entryBB = f.mkBasicBlock("entry");
  entryBB.set();
  
  auto entryBB1 = f.mkBasicBlock("entry1");
  cg.mkBranch(entryBB1);
  entryBB1.set();

#if 1
  auto res = cg.mkAlloca(cg.mkInt(0));
  auto val = cg.mkInt(100);
  auto cnd = f.arg(0) < val;
  cnd.ifThenElse(
      [&]() {
        auto then_val = f.arg(0) + cg.mkInt(1);
        res.store(then_val);
      },
      [&]() {
        auto else_val = f.arg(1) + cg.mkInt(2);
        res.store(else_val);
      });

  //auto phi = res.load();

//  auto f1 = [&](IR::BasicBlock bb, IR::Value iv) { 
//    res.store(res.load() + iv + phi);
//    return bb;
//  };
//  auto last = cg.mkLoop(cg.mkInt(0), phi, cg.mkInt(1), f1);
 // auto cmp = last != cg.mkInt(32);
#endif

  auto sum = cg.mkAlloca(cg.mkFloat(0));
#if 1
#if 1
  cg.mkNdLoop(
      {std::make_tuple(cg.mkInt(0), cg.mkInt(4), cg.mkInt(1)),
       std::make_tuple(cg.mkInt(0), cg.mkInt(5), cg.mkInt(1)),
       std::make_tuple(cg.mkInt(0), cg.mkInt(7), cg.mkInt(1))},
      [&](std::vector<IR::Value> iv) {
        auto tmp = iv[0] * iv[1]*iv[2];
        sum.store(sum.load() + tmp.castTo(cg.mkFloatTy()));
      });
#elif 0
  cg.mkNdLoop(
      {std::make_tuple(cg.mkInt(0), cg.mkInt(15), cg.mkInt(1))},
      [&](std::vector<IR::Value> iv) { sum.store(sum.load() + iv[0]); });
#elif 1
  cg.mkLoop(
      cg.mkInt(0), cg.mkInt(15), cg.mkInt(1),
      [&](IR::Value iv) { sum.store(sum.load() + iv); });
#else
  auto exitBB = f.mkBasicBlock("exit");
  cg.mkLoopWithExitBlock(cg.mkInt(0), cg.mkInt(15), cg.mkInt(1),
                         exitBB,
                         [&](IR::Value iv) {
                           sum.store(sum.load() + iv.castTo(cg.mkFloatTy()));
                         });
  exitBB.set();
#endif
#endif

  auto bb1 = cg.mkBasicBlock("BBX1");
  auto bb2 = cg.mkBasicBlock("BBX2");
  cg.mkBranch(bb1);

  bb1.set();
  cg.mkBranch(bb2);

  bb2.set();

  cg.mkRet(sum.load()); //cmp.mkSelect(last, sum.load()));
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
