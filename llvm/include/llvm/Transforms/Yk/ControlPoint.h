#ifndef LLVM_TRANSFORMS_YK_CONTROLPOINT_H
#define LLVM_TRANSFORMS_YK_CONTROLPOINT_H

#include "llvm/IR/PassManager.h"

namespace llvm {
    class ModulePass;

    class YkControlPointPass : public PassInfoMixin<YkControlPointPass> {
        public:
            PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
    };

    ModulePass *createYkControlPointPass();
}

#endif

