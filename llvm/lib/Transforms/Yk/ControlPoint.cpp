#include "llvm/Transforms/Yk/ControlPoint.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Dominators.h>

#define DEBUG_TYPE "yk-control-point"

using namespace llvm;

/// Given a Module, find the call to the control point we want to patch.
/// FIXME For now assumes there's only one control point.
CallInst *findControlPointCall(Module &M) {
  Function *CP = M.getFunction("control_point");
  assert(CP->user_begin() != CP->user_end());
  User *U = *CP->user_begin();
  return cast<CallInst>(U);
}

ConstantInt *make_cint(LLVMContext &Context, int bits, int value) {
  return ConstantInt::get(Context , APInt(bits, value));
}

void createControlPoint(Module &Mod, Function *F, std::vector<Value *> inputs, StructType *OutSTy) {

  auto &Context = Mod.getContext();
  //Function *Main = Mod.getFunction("main");

  // Create entry block and setup builder.
  auto CPEntry = BasicBlock::Create(Context, "cpentry", F);
  auto BBTracing = BasicBlock::Create(Context, "bbtracing", F);
  auto BBNotTracing = BasicBlock::Create(Context, "bbnottracing", F);
  auto BBHasTrace = BasicBlock::Create(Context, "bbhastrace", F);
  auto BBExecuteTrace = BasicBlock::Create(Context, "bbhastrace", F);
  auto BBHasNoTrace = BasicBlock::Create(Context, "bbhasnotrace", F);
  auto BBReturn = BasicBlock::Create(Context, "bbreturn", F);
  auto BBStopTracing = BasicBlock::Create(Context, "bbstoptracing", F);

  IRBuilder<> Builder(CPEntry);

  // Create tracing global
  ConstantInt *Int0 = ConstantInt::get(Context , APInt(8, 0));

  auto FuncStartTracing = llvm::Function::Create(FunctionType::get(Type::getVoidTy(Context),
      {Type::getInt64Ty(Context)}, false),
      GlobalValue::ExternalLinkage, "__yktrace_start_tracing", Mod);

  auto FuncStopTracing = llvm::Function::Create(FunctionType::get(Type::getInt8PtrTy(Context),
      {}, false),
      GlobalValue::ExternalLinkage, "__yktrace_stop_tracing", Mod);

  auto FuncCompileTrace = llvm::Function::Create(FunctionType::get(Type::getInt8PtrTy(Context),
      {Type::getInt8PtrTy(Context)}, false),
      GlobalValue::ExternalLinkage, "__yktrace_irtrace_compile", Mod);

  GlobalVariable *GVTracing = new GlobalVariable(
      Mod, Type::getInt8Ty(Context), false,
      GlobalVariable::InternalLinkage, Int0, "tracing2",
      (GlobalVariable *)nullptr);

  Constant * PtNull = Constant::getNullValue(Type::getInt8PtrTy(Context));
  GlobalVariable *GVCompiledTrace = new GlobalVariable(
      Mod, Type::getInt8PtrTy(Context), false,
      GlobalVariable::InternalLinkage, PtNull, "compiled_trace2",
      (GlobalVariable *)nullptr);

  GlobalVariable *GVStartLoc = new GlobalVariable(
      Mod, Type::getInt32Ty(Context), false,
      GlobalVariable::InternalLinkage, make_cint(Context, 32, -1), "start_loc2",
      (GlobalVariable *)nullptr);

  // Create control point entry block
  Value *GVTracingVal = Builder.CreateLoad(GVTracing);
  Value *Cond = Builder.CreateICmp(CmpInst::Predicate::ICMP_EQ, GVTracingVal, Int0);
  // Create output struct
  Value *OutS = F->getArg(1);
  Builder.CreateCondBr(Cond, BBNotTracing, BBTracing);

  // Create block for not tracing.
  Builder.SetInsertPoint(BBNotTracing);
  Value *GVCompiledTraceVal = Builder.CreateLoad(GVCompiledTrace);
  Value *Cond2 = Builder.CreateICmp(CmpInst::Predicate::ICMP_EQ, GVCompiledTraceVal, PtNull);
  Builder.CreateCondBr(Cond2, BBHasNoTrace, BBHasTrace);

  // Create block that starts tracing.
  Builder.SetInsertPoint(BBHasNoTrace);
  ConstantInt *Int640 = ConstantInt::get(Context , APInt(64, 0));
  ConstantInt *Int641 = ConstantInt::get(Context , APInt(64, 1));
  Builder.CreateCall(FuncStartTracing->getFunctionType(), FuncStartTracing, {Int641});
  Builder.CreateStore(ConstantInt::get(Context , APInt(8, 1)), GVTracing);
  Builder.CreateStore(F->getArg(0), GVStartLoc);
  Builder.CreateBr(BBReturn);

  // Create block that executes a trace.
  Builder.SetInsertPoint(BBHasTrace);
  Value *ValStartLoc2 = Builder.CreateLoad(GVStartLoc);
  Value *ExecTraceCond = Builder.CreateICmp(CmpInst::Predicate::ICMP_EQ, ValStartLoc2, F->getArg(0));
  Builder.CreateCondBr(ExecTraceCond, BBExecuteTrace, BBReturn);

  Builder.SetInsertPoint(BBExecuteTrace);
  // FIXME Build type depending on INS/OUTS
  std::vector<Type *> TypeParams;
  for (Value *input : inputs) {
    TypeParams.push_back(input->getType());
  }



  FunctionType *FType = FunctionType::get(
      OutSTy, {OutSTy}, false);
  Value *CastTrace = Builder.CreateBitCast(GVCompiledTraceVal, FType->getPointerTo());
  //
  Function *Puts = Mod.getFunction("puts");
  assert(Puts != nullptr);
  Value *PutsString = Builder.CreateGlobalStringPtr(StringRef("Enter JIT code"));
  Builder.CreateCall(Puts, PutsString);
  CallInst *CTResult = Builder.CreateCall(FType, CastTrace, F->getArg(1));
  Value *PutsString2 = Builder.CreateGlobalStringPtr(StringRef("Exit JIT code"));
  Builder.CreateCall(Puts, PutsString2);
  //
  CTResult->setTailCall(true);
  Builder.CreateBr(BBReturn);

  // Create block deciding to stop tracing or not
  Builder.SetInsertPoint(BBTracing);
  Value *ValStartLoc = Builder.CreateLoad(GVStartLoc);
  Value *Cond3 = Builder.CreateICmp(CmpInst::Predicate::ICMP_EQ, ValStartLoc, F->getArg(0));
  Builder.CreateCondBr(Cond3, BBStopTracing, BBReturn);

  // Create stop tracing
  Builder.SetInsertPoint(BBStopTracing);
  Value *TR = Builder.CreateCall(FuncStopTracing->getFunctionType(), FuncStopTracing);
  // FIXME pass OUTS so we know what to return from the compiled trace
  Value *CT = Builder.CreateCall(FuncCompileTrace->getFunctionType(), FuncCompileTrace, {TR});
  Builder.CreateStore(CT, GVCompiledTrace);
  Builder.CreateStore(make_cint(Context, 8, 0), GVTracing);
  Builder.CreateBr(BBReturn);

  // Create Return Block
  Builder.SetInsertPoint(BBReturn);
  PHINode *phi = Builder.CreatePHI(OutSTy, 3);
  phi->addIncoming(OutS, BBHasTrace);
  phi->addIncoming(CTResult, BBExecuteTrace);
  phi->addIncoming(OutS, BBTracing);
  phi->addIncoming(OutS, BBHasNoTrace);
  phi->addIncoming(OutS, BBStopTracing);

  Builder.CreateRet(phi);
}

std::vector<Value *> getLiveValues(DominatorTree &DT, CallInst *OldCP) {
  errs() << "getting live values\n";
  std::vector<Value *> vec;
  Function *Func = OldCP->getFunction();
  for(auto &BB: *Func) {
    if (!DT.dominates(cast<Instruction>(OldCP), &BB)) {
      for (auto &I: BB) {
        if (DT.dominates(&I, cast<Instruction>(OldCP))) {
          if (!I.getType()->isVoidTy()) {
            I.dump();
            vec.push_back(&I);
          }
        }
      }
    }
  }
  errs() << "done\n";
  return vec;
}

namespace {
struct YkControlPointPass2 : public ModulePass {
    static char ID;

    YkControlPointPass2() : ModulePass(ID) {}

    StringRef getPassName() const override {
      return "Control Point Patching Pass";
    }

    bool runOnModule(Module &M) override {
        M.dump();
        errs() << "Patching control point.\n";
        LLVMContext &context = M.getContext();

        CallInst *OldCPCall = findControlPointCall(M);
        //OldCPCall->dump();

        //NF->dump();

        // Replace old control point call.
        IRBuilder<> Builder(OldCPCall);
        //Builder.SetInsertPoint(OldCPCall);

        // Find inputs/outputs (since we are in a loop these are the same)
        // FIXME do this using dominator tree
        Function *Main = M.getFunction("main");
        DominatorTree DT(*Main);

        std::vector<Value *> LiveVals = getLiveValues(DT, OldCPCall);

        std::vector<Type *> TypeParams;
        for (Value *V : LiveVals) {
            TypeParams.push_back(V->getType());
        }

        // Create new control point function definition.
        //std::vector<Type *> TypeParams = {Type::getInt32Ty(context)};
        auto CPReturnTy = StructType::create(TypeParams, "OutputStruct");
        FunctionType *FType = FunctionType::get(CPReturnTy, {Type::getInt32Ty(context), CPReturnTy} , false);
        Function *NF = Function::Create(FType, GlobalVariable::ExternalLinkage, "new_control_point", M);

        // FIXME get first arg from old controlpoint
        // Create Input struct
        errs() << "createinput struct\n";
        Value *InputStruct = cast<Value>(Constant::getNullValue(CPReturnTy));
        for (uint64_t i = 0; i < LiveVals.size(); i++) {
          InputStruct = Builder.CreateInsertValue(InputStruct, LiveVals[i], {i});
        }

        CallInst *CPRet = Builder.CreateCall(NF, {OldCPCall->getArgOperand(0), InputStruct});
        // Replace all future references of pc with newpc.
        errs() << "extrave values from contro lpoint\n";
        for (uint64_t i = 0; i < LiveVals.size(); i++) {
          Value *New = Builder.CreateExtractValue(cast<Value>(CPRet), i);
          LiveVals[i]->replaceUsesWithIf(New, [&](Use &U) {return DT.dominates(CPRet, U);});
        }

        OldCPCall->eraseFromParent();
        createControlPoint(M, NF, LiveVals, CPReturnTy);

        M.dump();
        verifyModule(M, &errs());
        return true;
    }
};
char YkControlPointPass2::ID = 0;
}


PreservedAnalyses YkControlPointPass::run(Module &M,
                                          ModuleAnalysisManager &AM) {
  return PreservedAnalyses::none();
}

namespace llvm {
ModulePass *createYkControlPointPass() {
  return new YkControlPointPass2();
}
}
