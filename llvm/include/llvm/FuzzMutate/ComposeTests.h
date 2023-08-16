#ifndef LLVM_FUZZMUTATE_COMPOSETESTS_H
#define LLVM_FUZZMUTATE_COMPOSETESTS_H

#include "llvm/FuzzMutate/RandomIRBuilder.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Linker/Linker.h"
#include <unordered_map>
#include <unordered_set>

namespace llvm {

class FunctionCombiner {
public:
  enum CrossTy { Inline, Sequence };
  FunctionCombiner(std::unique_ptr<Module> M, int Seed);
  FunctionCombiner(std::unique_ptr<Module> M1, std::unique_ptr<Module> M2,
                   int Seed);

protected:
  std::unique_ptr<Module> cloneDestM();
  std::unique_ptr<Module> cloneSrcM();
  void deleteGlobalVal(GlobalValue *GV);
  void getEnclosingFns(Value *V, SmallSet<Function *, 4> &Fns);
  int genRandNum(int Min, int Max);
  Value *getRandConstant(Type *Ty);

  // This function aggressively removes every unnecessary structure from
  // the module, such that only F and everything F depends upon remains
  void cleanModule(Module &M, Function *KeepF);
  void resolveGlobalSymbols();
  bool linkFunctionInto(Function *Func, Function **FuncPtr,
                        Module &CompositeM, Linker &L);

  // Generates a metadata node for each function that stores the source
  // function file name
  void initializeMetadata(Module &M);

  // Inserts the named metadata node stored in this class into the
  // module argument.
  void insertCrossoverMetadata(Module &M, Function *F1, Function *F2,
                               Function *NewF, CrossTy T);

  LLVMContext &Context;
  const std::unique_ptr<Module> DestM;
  const std::unique_ptr<Module> SrcM;
  SmallVector<Function *, 4> DestFuncs;
  SmallVector<Function *, 4> SrcFuncs;
  RandomIRBuilder RIRB;
};

class FunctionInliner : public FunctionCombiner {
public:
  using FunctionCombiner::FunctionCombiner;

  std::unique_ptr<Module> inlineFunctions();

private:
  using TypeToValuesMap = std::unordered_map<Type *,
                                             std::unordered_set<Value *>>;

  /// Get all LLVM Values that are defined before the Inst parameter
  /// and are accessible in the scope of the Inst and store them in Defns.
  void getArgCandidates(Instruction *Inst, TypeToValuesMap &Defns);
  bool inlineFunction(
      Instruction *DestInst, Function *SrcFunc,
      SmallVector<unsigned, 4> &DestInstOperands, TypeToValuesMap &Defns);
};

class FunctionSequencer : public FunctionCombiner {
public:
  using FunctionCombiner::FunctionCombiner;

  std::unique_ptr<Module> sequenceFunctions();

private:
  bool isUnsupportedArgType(Type *Ty);

  // Define some sequencing strategies that we can randomly pick from during
  // mutation
  class SeqStrategy {
  public:
    using ArgList = SmallVector<Value *, 4>;
    using ArgTypeSet = std::unordered_set<Type *>;
    SeqStrategy(FunctionSequencer &FC) : Parent(FC) {}
    virtual ~SeqStrategy() = default;
    virtual bool trySequence(Function *F1, Function *F2,
                             Function **CallerFunc, Value **CallerRetVal,
                             Type **CallerRetValType, IRBuilder<> &IRB) {
      llvm_unreachable("Strategy does not implement any mutators");
    }
  protected:
    Function *createEmptyFunction(Module &Dest, Type *RetTy);
    void getArgTypes(Function *F, ArgTypeSet &Types);
    void getRandConstArgs(Function *F, ArgList &Args, Value *ArgOverride);

    FunctionSequencer &Parent;
  };
  class RetValSeq : public SeqStrategy {
  public:
    using SeqStrategy::SeqStrategy;
    // Try to insert the returned value of one function into the other,
    // then return the second function's value if its not null
    bool trySequence(Function *F1, Function *F2,
                     Function **CallerFunc, Value **CallerRetVal,
                     Type **CallerRetValType, IRBuilder<> &IRB) override;
  };
  class PtrArgSeq : public SeqStrategy {
  public:
    using SeqStrategy::SeqStrategy;
    // See if both functions have a matching pointer type
    // argument so that we can pass the same variable to both functions
    bool trySequence(Function *F1, Function *F2,
                     Function **CallerFunc, Value **CallerRetVal,
                     Type **CallerRetValType, IRBuilder<> &IRB) override;
  };
  class BinOpSeq : public SeqStrategy {
  public:
    using SeqStrategy::SeqStrategy;
    // If both functions have a matching float/int return type
    // then try to merge data flow by combining the two function return
    // values by passing them into a binary op and returning the resultant
    // value for the CallerFunc
    bool trySequence(Function *F1, Function *F2,
                     Function **CallerFunc, Value **CallerRetVal,
                     Type **CallerRetValType, IRBuilder<> &IRB) override;
  };
  class VoidSeq : public SeqStrategy {
  public:
    using SeqStrategy::SeqStrategy;
    // If the above 3 cases cannot be satified, then simply add two function
    // calls in the same function without any data flow injection and return
    // void
    bool trySequence(Function *F1, Function *F2,
                     Function **CallerFunc, Value **CallerRetVal,
                     Type **CallerRetValType, IRBuilder<> &IRB) override;
  };
};


} // namespace llvm

#endif // LLVM_FUZZMUTATE_COMPOSETESTS_H
