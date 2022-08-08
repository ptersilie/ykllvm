//===- Stackmaps.cpp - Add Stackmaps before branches and calls ------------===//
//
// Add stackmap calls at each point that could potentially lead to a guard
// failure inside the JIT. The information collected by those calls allows us
// to recreate the stack after a guard failure and return straight back to the
// interpreter.

#include "llvm/Transforms/Yk/Stackmaps.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "LivenessAnalysis.h"
#include <map>

#define DEBUG_TYPE "yk-stackmaps"

using namespace llvm;

namespace llvm {

void initializeYkStackmapsPass(PassRegistry &);
} // namespace llvm

namespace {
class YkStackmaps : public ModulePass {
public:
  static char ID;
  YkStackmaps() : ModulePass(ID) {
    initializeYkStackmapsPass(*PassRegistry::getPassRegistry());
  }

  bool runOnModule(Module &M) override {
    LLVMContext &Context = M.getContext();

    Intrinsic::ID SMFuncID = Function::lookupIntrinsicID("llvm.experimental.stackmap");
    if (SMFuncID == Intrinsic::not_intrinsic) {
      Context.emitError("can't find stackmap()");
      return false;
    }

    Function *SMFunc = Intrinsic::getDeclaration(&M, SMFuncID);
    std::map<Instruction *, std::set<Value *>> SMCalls;
    for (Function &F: M) {
      LivenessAnalysis LA(&F);
      for (BasicBlock &BB: F) {
        for (Instruction &I: BB) {
          if ((isa<CallInst>(I)) || ((isa<BranchInst>(I)) && (cast<BranchInst>(I).isConditional())) || isa<SwitchInst>(I)) {
            SMCalls.insert({&I, LA.getLiveVarsBefore(&I)});
          }
        }
      }
    }

    uint64_t Count = 0;
    uint64_t LiveCount = 0;
    Value *Shadow = ConstantInt::get(Type::getInt32Ty(Context), 0);
    for (auto It: SMCalls) {
      Instruction *I = cast<Instruction>(It.first);
      std::set<Value *> L = It.second;

      IRBuilder<> Bldr(I);
      Value *SMID = ConstantInt::get(Type::getInt64Ty(Context), Count);
      std::vector<Value *> Args = {SMID, Shadow};
      for (Value *A: L) {
        Type *TheTy = A->getType();
        Args.push_back(A);
        LiveCount++;
      }
      assert(SMFunc != nullptr);
      Bldr.CreateCall(SMFunc->getFunctionType(), SMFunc, ArrayRef<Value *>(Args));
      Count++;
    }


#ifndef NDEBUG
    // Our pass runs after LLVM normally does its verify pass. In debug builds
    // we run it again to check that our pass is generating valid IR.
    if (verifyModule(M, &errs())) {
      Context.emitError("Control point pass generated invalid IR!");
      return false;
    }
#endif
    return true;
  }
};
} // namespace

char YkStackmaps::ID = 0;
INITIALIZE_PASS(YkStackmaps, DEBUG_TYPE, "yk stackmaps", false, false)

ModulePass *llvm::createYkStackmapsPass() { return new YkStackmaps(); }
