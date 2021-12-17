//===- ControlPoint.cpp - Synthesise the yk control point -----------------===//
//
// This pass finds the user's call to the dummy control point and replaces it
// with a call to a new control point that implements the necessary logic to
// drive the yk JIT.
//
// The pass converts an interpreter loop that looks like this:
//
// ```
// pc = 0;
// while (...) {
//     yk_control_point(); // <- dummy control point
//     bc = program[pc];
//     switch (bc) {
//         // bytecode handlers here.
//     }
// }
// ```
//
// Into one that looks like this (note that this transformation happens at the
// IR level):
//
// ```
// // The YkCtrlPointStruct contains one member for each live LLVM variable
// // just before the call to the control point.
// struct YkCtrlPointStruct {
//     size_t pc;
// }
//
// pc = 0;
// while (...) {
//     struct YkCtrlPointStruct cp_in = { pc };
//     // Now we call the patched control point.
//     YkCtrlPointStruct cp_out = yk_new_control_point(cp_in);
//     pc = cp_out.pc;
//     bc = program[pc];
//     switch (bc) {
//         // bytecode handlers here.
//     }
// }
// ```
//
// The call to the dummy control point must be the first thing that appears in
// an interpreter dispatch loop.
//
// YKFIXME: The control point cannot yet be used in an interpreter using
// threaded dispatch.
//
// YKFIXME: The tracing logic is currently over-simplified. The following items
// need to be fixed:
//
//  - The address of `YkLocation` instances are used for identity, but they are
//    intended to be freely moved by the user.
//
//  - Tracing starts when we encounter a location for which we have no machine
//    code. A hot counter should be used instead.
//
//  - There can be only one compiled trace for now. There should be a code
//    cache mapping from JIT locations to their machine code.
//
//  - The interpreter is assumed to be single threaded. We should implement a
//    synchronisation function in Rust code that synchronises many threads which
//    are calling the control point concurrently. This function should return a
//    value that indicates if we should start/stop tracing, or jump to machine
//    code etc.
//
//  - Guards are currently assumed to abort the program.
//    https://github.com/ykjit/yk/issues/443
//
//  - The block that performs the call to JITted code branches back to itself
//    to achieve rudimentary trace stitching. The looping should really be
//    implemented in the JITted code itself so that it isn't necessary to
//    repeatedly enter and exit the JITted code.
//    https://github.com/ykjit/yk/issues/442
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Yk/ControlPoint.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include <llvm/IR/Dominators.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>

#define DEBUG_TYPE "yk-control-point"
#define JIT_STATE_PREFIX "jit-state: "

// These constants mirror `ykrt::mt::JITACTION_*`.
const uintptr_t JITActionNop = 1;
const uintptr_t JITActionStartTracing = 2;
const uintptr_t JITActionStopTracing = 3;

using namespace llvm;

/// Find the call to the dummy control point that we want to patch.
/// Returns either a pointer the call instruction, or `nullptr` if the call
/// could not be found.
/// YKFIXME: For now assumes there's only one control point.
CallInst *findControlPointCall(Module &M) {
  // Find the declaration of `yk_control_point()`.
  Function *CtrlPoint = M.getFunction(YK_DUMMY_CONTROL_POINT);
  if (CtrlPoint == nullptr)
    return nullptr;

  // Find the call site of `yk_control_point()`.
  Value::user_iterator U = CtrlPoint->user_begin();
  if (U == CtrlPoint->user_end())
    return nullptr;

  return cast<CallInst>(*U);
}

/// Creates a call for printing debug information inside the control point.
void createJITStatePrint(IRBuilder<> &Builder, Module *Mod, std::string Str) {
  if (std::getenv("YKD_PRINT_JITSTATE") == nullptr)
    return;
  LLVMContext &Context = Mod->getContext();
  FunctionCallee Puts = Mod->getOrInsertFunction(
      "__yk_debug_print",
      FunctionType::get(Type::getVoidTy(Context),
                        PointerType::get(Type::getInt8Ty(Context), 0), true));
  Value *PutsString =
      Builder.CreateGlobalStringPtr(StringRef(JIT_STATE_PREFIX + Str));
  Builder.CreateCall(Puts, PutsString);
}

/// Generates the new control point, which includes all logic to start/stop
/// tracing and to compile/execute traces.
void createControlPoint(Module &Mod, Function *F, std::vector<Value *> LiveVars,
                        StructType *YkCtrlPointStruct, Type *YkLocTy) {
  auto &Context = Mod.getContext();

  // Create control point blocks and setup the IRBuilder.
  BasicBlock *CtrlPointEntry = BasicBlock::Create(Context, "cpentry", F);
  BasicBlock *BBExecuteTrace = BasicBlock::Create(Context, "bbhexectrace", F);
  BasicBlock *BBStartTracing = BasicBlock::Create(Context, "bbstarttracing", F);
  BasicBlock *BBReturn = BasicBlock::Create(Context, "bbreturn", F);
  BasicBlock *BBStopTracing = BasicBlock::Create(Context, "bbstoptracing", F);

  // Get the type for a pointer-sized integer.
  DataLayout DL(&Mod);
  unsigned PtrBitSize = DL.getPointerSize() * 8;
  IntegerType *PtrSizedInteger = IntegerType::getIntNTy(Context, PtrBitSize);

  // Some frequently used constants.
  ConstantInt *JActNop = ConstantInt::get(PtrSizedInteger, JITActionNop);
  ConstantInt *JActStartTracing =
      ConstantInt::get(PtrSizedInteger, JITActionStartTracing);
  ConstantInt *JActStopTracing =
      ConstantInt::get(PtrSizedInteger, JITActionStopTracing);

  // Add definitions for __yk functions.
  Function *FuncTransLoc = llvm::Function::Create(
      FunctionType::get(PtrSizedInteger, {Type::getInt8PtrTy(Context)}, false),
      GlobalValue::ExternalLinkage, "__ykrt_transition_location", Mod);

  Function *FuncSetCodePtr = llvm::Function::Create(
      FunctionType::get(
          Type::getVoidTy(Context),
          {Type::getInt8PtrTy(Context), Type::getInt8PtrTy(Context)}, false),
      GlobalValue::ExternalLinkage, "__ykrt_set_loc_code_ptr", Mod);

  Function *FuncStartTracing = llvm::Function::Create(
      FunctionType::get(Type::getVoidTy(Context), {Type::getInt64Ty(Context)},
                        false),
      GlobalValue::ExternalLinkage, "__yktrace_start_tracing", Mod);

  Function *FuncStopTracing = llvm::Function::Create(
      FunctionType::get(Type::getInt8PtrTy(Context), {}, false),
      GlobalValue::ExternalLinkage, "__yktrace_stop_tracing", Mod);

  Function *FuncCompileTrace = llvm::Function::Create(
      FunctionType::get(Type::getInt8PtrTy(Context),
                        {Type::getInt8PtrTy(Context)}, false),
      GlobalValue::ExternalLinkage, "__yktrace_irtrace_compile", Mod);

  // Populate the entry block. This calls `__ykrt_transition_location()` to
  // decide what to do next.
  IRBuilder<> Builder(CtrlPointEntry);
  Value *CastLoc =
      Builder.CreateBitCast(F->getArg(0), Type::getInt8PtrTy(Context));
  Value *JITAction = Builder.CreateCall(FuncTransLoc->getFunctionType(),
                                        FuncTransLoc, {CastLoc});
  SwitchInst *ActionSw = Builder.CreateSwitch(JITAction, BBExecuteTrace, 3);
  ActionSw->addCase(JActNop, BBReturn);
  ActionSw->addCase(JActStartTracing, BBStartTracing);
  ActionSw->addCase(JActStopTracing, BBStopTracing);

  // Populate the block that starts tracing.
  Builder.SetInsertPoint(BBStartTracing);
  createJITStatePrint(Builder, &Mod, "start-tracing");
  Builder.CreateCall(FuncStartTracing->getFunctionType(), FuncStartTracing,
                     {ConstantInt::get(Context, APInt(64, 1))});
  Builder.CreateBr(BBReturn);

  // Populate the block that calls a compiled trace. If execution gets into
  // this block then `JITAction` is a pointer to a compiled trace.
  Builder.SetInsertPoint(BBExecuteTrace);
  std::vector<Type *> TypeParams;
  for (Value *LV : LiveVars) {
    TypeParams.push_back(LV->getType());
  }
  FunctionType *FType = FunctionType::get(
      Type::getVoidTy(Context), {YkCtrlPointStruct->getPointerTo(), Type::getInt8PtrTy(Context), Type::getInt64Ty(Context)}, false);

  // XXX use PtrSizedInteger
  Value *JITActionPtr =
      Builder.CreateIntToPtr(JITAction, Type::getInt64PtrTy(Context));
  // Extract trace pointer.
  Value *TraceValPtr = Builder.CreateGEP(Type::getInt64Ty(Context), JITActionPtr, Builder.getInt32(0));
  Value *TraceVal = Builder.CreateLoad(Type::getInt64Ty(Context), TraceValPtr);
  Value *TracePtr = Builder.CreateIntToPtr(TraceVal, Type::getInt8PtrTy(Context));
  // Extract stackmap pointer.
  Value *StackMapValPtr = Builder.CreateGEP(Type::getInt64Ty(Context), JITActionPtr, Builder.getInt32(1));
  Value *StackMapVal = Builder.CreateLoad(Type::getInt64Ty(Context), StackMapValPtr);
  Value *StackMapPtr = Builder.CreateIntToPtr(StackMapVal, Type::getInt8PtrTy(Context));
  // Extract stackmap size.
  Value *StackMapSizePtr = Builder.CreateGEP(Type::getInt64Ty(Context), JITActionPtr, Builder.getInt32(2));
  Value *StackMapSize = Builder.CreateLoad(Type::getInt64Ty(Context), StackMapSizePtr);

  Value *CastTrace = Builder.CreateBitCast(TracePtr, FType->getPointerTo());
  createJITStatePrint(Builder, &Mod, "enter-jit-code");
  CallInst *CTResult = Builder.CreateCall(FType, CastTrace, {F->getArg(1), StackMapPtr, StackMapSize});
  createJITStatePrint(Builder, &Mod, "exit-jit-code");
  CTResult->setTailCall(true);
  Builder.CreateBr(BBExecuteTrace);

  // Create block that stops tracing, compiles a trace, and stores it in a
  // global variable.
  Builder.SetInsertPoint(BBStopTracing);
  Value *TR =
      Builder.CreateCall(FuncStopTracing->getFunctionType(), FuncStopTracing);
  Value *CT = Builder.CreateCall(FuncCompileTrace->getFunctionType(),
                                 FuncCompileTrace, {TR});
  Builder.CreateCall(FuncSetCodePtr->getFunctionType(), FuncSetCodePtr,
                     {CastLoc, CT});
  createJITStatePrint(Builder, &Mod, "stop-tracing");
  Builder.CreateBr(BBReturn);

  // Populate the return block.
  Builder.SetInsertPoint(BBReturn);
  Builder.CreateRetVoid();
}

/// Extract all live variables that need to be passed into the control point.
std::vector<Value *> getLiveVars(DominatorTree &DT, CallInst *OldCtrlPoint) {
  std::vector<Value *> Vec;
  Function *Func = OldCtrlPoint->getFunction();
  for (auto &BB : *Func) {
    if (!DT.dominates(cast<Instruction>(OldCtrlPoint), &BB)) {
      for (auto &I : BB) {
        if ((!I.getType()->isVoidTy()) &&
            (DT.dominates(&I, cast<Instruction>(OldCtrlPoint)))) {
          Vec.push_back(&I);
        }
      }
    }
  }
  return Vec;
}

namespace llvm {
void initializeYkControlPointPass(PassRegistry &);
}

namespace {
class YkControlPoint : public ModulePass {
public:
  static char ID;
  YkControlPoint() : ModulePass(ID) {
    initializeYkControlPointPass(*PassRegistry::getPassRegistry());
  }

  bool runOnModule(Module &M) override {
    LLVMContext &Context = M.getContext();

    // Locate the "dummy" control point provided by the user.
    CallInst *OldCtrlPointCall = findControlPointCall(M);
    if (OldCtrlPointCall == nullptr) {
      Context.emitError(
          "ykllvm couldn't find the call to `yk_control_point()`");
      return false;
    }

    // Get function containing the control point.
    Function *Caller = OldCtrlPointCall->getFunction();

    // Find all live variables just before the call to the control point.
    DominatorTree DT(*Caller);
    std::vector<Value *> LiveVals = getLiveVars(DT, OldCtrlPointCall);
    if (LiveVals.size() == 0) {
      Context.emitError(
          "The interpreter loop has no live variables!\n"
          "ykllvm doesn't support this scenario, as such an interpreter would "
          "make little sense.");
      return false;
    }

    // Generate the YkCtrlPointVars struct. This struct is used to package up a
    // copy of all LLVM variables that are live just before the call to the
    // control point. These are passed in to the patched control point so that
    // they can be used as inputs and outputs to JITted trace code. The control
    // point returns a new YkCtrlPointVars whose members may have been mutated
    // by JITted trace code (if a trace was executed).
    std::vector<Type *> TypeParams;
    for (Value *V : LiveVals) {
      TypeParams.push_back(V->getType());
    }
    StructType *CtrlPointVarsTy =
        StructType::create(TypeParams, "YkCtrlPointVars");

    // Create the new control point.
    Type *YkLocTy = OldCtrlPointCall->getArgOperand(0)->getType();
    FunctionType *FType =
        FunctionType::get(Type::getVoidTy(Context),
                          {YkLocTy, CtrlPointVarsTy->getPointerTo()}, false);
    Function *NF = Function::Create(FType, GlobalVariable::ExternalLinkage,
                                    YK_NEW_CONTROL_POINT, M);

    // At the top of the function, instantiate a `YkCtrlPointStruct` to pass in
    // to the control point. We do so on the stack, so that we can pass the
    // struct by pointer.
    IRBuilder<> Builder(Caller->getEntryBlock().getFirstNonPHI());
    Value *InputStruct = Builder.CreateAlloca(CtrlPointVarsTy, 0, "");

    Builder.SetInsertPoint(OldCtrlPointCall);
    unsigned LvIdx = 0;
    for (Value *LV : LiveVals) {
      Value *FieldPtr =
          Builder.CreateGEP(CtrlPointVarsTy, InputStruct,
                            {Builder.getInt32(0), Builder.getInt32(LvIdx)});
      Builder.CreateStore(LV, FieldPtr);
      assert(LvIdx != UINT_MAX);
      LvIdx++;
    }

    // Insert call to the new control point.
    Instruction *NewCtrlPointCallInst = Builder.CreateCall(
        NF, {OldCtrlPointCall->getArgOperand(0), InputStruct});

    // Once the control point returns we need to extract the (potentially
    // mutated) values from the returned YkCtrlPointStruct and reassign them to
    // their corresponding live variables. In LLVM IR we can do this by simply
    // replacing all future references with the new values.
    LvIdx = 0;
    for (Value *LV : LiveVals) {
      Value *FieldPtr =
          Builder.CreateGEP(CtrlPointVarsTy, InputStruct,
                            {Builder.getInt32(0), Builder.getInt32(LvIdx)});
      Value *New = Builder.CreateLoad(TypeParams[LvIdx], FieldPtr);
      LV->replaceUsesWithIf(
          New, [&](Use &U) { return DT.dominates(NewCtrlPointCallInst, U); });
      assert(LvIdx != UINT_MAX);
      LvIdx++;
    }

    // Replace the call to the dummy control point.
    OldCtrlPointCall->eraseFromParent();

    // Generate new control point logic.
    createControlPoint(M, NF, LiveVals, CtrlPointVarsTy, YkLocTy);
    return true;
  }
};
} // namespace

char YkControlPoint::ID = 0;
INITIALIZE_PASS(YkControlPoint, DEBUG_TYPE, "yk control point", false, false)

ModulePass *llvm::createYkControlPointPass() { return new YkControlPoint(); }
