//===-- ComposeTests.cpp - Example Transformations --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/FuzzMutate/ComposeTests.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/ProfileSummaryInfo.h"
#include "llvm/FuzzMutate/Random.h"
#include "llvm/FuzzMutate/RandomIRBuilder.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <random>
#include <fstream>
#include <sys/stat.h>

#define DEBUG
#ifdef DEBUG
#define DEBUG_PRINT(x) x
#else
#define DEBUG_PRINT(x)
#endif

// # define LOG_CSV_STATS

using namespace llvm;

template<typename T>
static void shuffleVector(SmallVector<T, 4> &V) {
  std::random_device RD;
  auto RNG = std::default_random_engine { RD() };
  std::shuffle(std::begin(V), std::end(V), RNG);
}

#ifdef LOG_CSV_STATS
// Write to a csv:
// - the source function
// - the destination function
// - the instruction that the source function was inlined into
// - whether the destination function introduces new control flow
//   (i.e. contains branch instructions)
static void writeToCSV(Function *SrcFunc, Instruction *DstInst) {
  Function *DstFunc = DstInst->getFunction();

  auto FileExists = [](std::string &Filename) -> bool {
    struct stat buffer;
    return (stat (Filename.c_str(), &buffer) == 0);
  };

  std::string CurrWorkPath = std::getenv("PWD");
  std::string OutputCSV = CurrWorkPath + "/mutation.csv";

  std::ofstream CSVFile;

  if (FileExists(OutputCSV)) {
    CSVFile.open(OutputCSV, std::ofstream::app);
  } else {
    CSVFile.open(OutputCSV, std::ofstream::trunc);
    CSVFile << "pair_id,src_func,dest_func,insert_inst,has_branch\n";
  }

  std::string PairID = DstFunc->getParent()->getModuleIdentifier();
  size_t FilenameStartIndex = PairID.find_last_of("/");
  size_t ExtIndex = PairID.find_last_of(".");
  assert(ExtIndex != std::string::npos);
  if (FilenameStartIndex == std::string::npos) {
    FilenameStartIndex = 0;
  } else {
    FilenameStartIndex++;
  }
  PairID = PairID.substr(FilenameStartIndex,
                         ExtIndex - FilenameStartIndex);
  PairID = PairID + "_crossed";
  CSVFile << PairID << ",";
  CSVFile << SrcFunc->getName().str() << ",";
  CSVFile << DstFunc->getName().str() << ",";

  std::string DstInstStr;
  llvm::raw_string_ostream SS(DstInstStr);
  SS << *DstInst;

  // Replace quotations with a double quotation to allow the instruction
  // field to be read in as a CSV
  auto ReplaceQuotation = [](std::string &S) -> void {
    size_t StartPos = 0;
    while((StartPos = S.find("\"", StartPos)) != std::string::npos) {
      S.replace(StartPos, 1, "\"\"");
      StartPos += 2;
    }
  };
  ReplaceQuotation(DstInstStr);
  CSVFile << "\"" << DstInstStr << "\",";

  bool HasControlFlow = false;
  for (BasicBlock &BB : *SrcFunc) {
    for (Instruction &I : BB) {
      HasControlFlow |= isa<BranchInst>(I);
    }
  }
  CSVFile << HasControlFlow << "\n";
  CSVFile.close();
}
#endif

FunctionCombiner::FunctionCombiner(std::unique_ptr<Module> M, int Seed)
    : Context(M->getContext()), DestM(std::move(M)), SrcM(nullptr),
      RIRB(RandomIRBuilder(Seed, {})) {

  DEBUG_PRINT(dbgs() << "Initializing FunctionCombiner with module:\n";)
  DEBUG_PRINT(dbgs() << "M:" << DestM->getSourceFileName() << "\n";)

  for (Function &F : *DestM) {
    // Skip functions without a body
    if (F.size() > 0) {
      DestFuncs.push_back(&F);
      SrcFuncs.push_back(&F);
    }
  }

  assert(DestFuncs.size() != 0 && "Module M doesn't contain any functions!");

  shuffleVector<Function *>(DestFuncs);
  shuffleVector<Function *>(SrcFuncs);
}

FunctionCombiner::FunctionCombiner(std::unique_ptr<Module> M1,
                                   std::unique_ptr<Module> M2, int Seed)
    : Context(M1->getContext()), DestM(std::move(M1)), SrcM(std::move(M2)),
      RIRB(RandomIRBuilder(Seed, {})) {

  DEBUG_PRINT(dbgs() << "Initializing FunctionCombiner with modules:\n";)
  DEBUG_PRINT(dbgs() << "M1:" << DestM->getSourceFileName() << "\n";)
  DEBUG_PRINT(dbgs() << "M2:" << SrcM->getSourceFileName() << "\n";)

  // Ensure that the two modules are loaded into the same context,
  // otherwise linking will not work!
  assert(&DestM->getContext() == &SrcM->getContext());

  auto CollectUsableFunctions = [](SmallVector<Function *, 4> &Funcs,
                                   Module &M) -> void {
    for (Function &F : M) {
      // Skip functions without a body
      if (F.size() > 0) {
        Funcs.push_back(&F);
      }
    }
  };

  CollectUsableFunctions(DestFuncs, *DestM);
  CollectUsableFunctions(SrcFuncs, *SrcM);

  assert(DestFuncs.size() != 0 && "Module M1 doesn't contain any functions!");
  assert(SrcFuncs.size() != 0 && "Module M2 doesn't contain any functions!");

  shuffleVector<Function *>(DestFuncs);
  shuffleVector<Function *>(SrcFuncs);
}

int FunctionCombiner::genRandNum(int Min, int Max) {
  std::uniform_int_distribution<int> Uniform(Min, Max);
  return Uniform(RIRB.Rand);
}

Value *FunctionCombiner::getRandConstant(Type *Ty) {
  auto RS = makeSampler<Value *>(RIRB.Rand);
  SmallVector<Constant *, 6> Cs;

  switch (Ty->getTypeID()) {
  case Type::IntegerTyID: {
    auto *IntTy = dyn_cast<IntegerType>(Ty);
    uint64_t W = IntTy->getBitWidth();
    Cs.push_back(ConstantInt::get(IntTy, APInt::getMaxValue(W)));
    Cs.push_back(ConstantInt::get(IntTy, APInt::getMinValue(W)));
    Cs.push_back(ConstantInt::get(IntTy, APInt::getSignedMaxValue(W)));
    Cs.push_back(ConstantInt::get(IntTy, APInt::getSignedMinValue(W)));
    Cs.push_back(ConstantInt::get(IntTy, APInt::getOneBitSet(W, W / 2)));
    Cs.push_back(ConstantInt::get(IntTy, 0));
    break;
  }
  case Type::HalfTyID:
  case Type::BFloatTyID:
  case Type::FloatTyID:
  case Type::DoubleTyID:
  case Type::X86_FP80TyID:
  case Type::FP128TyID:
  case Type::PPC_FP128TyID: {
    auto &Ctx = Ty->getContext();
    auto &Sem = Ty->getFltSemantics();
    Cs.push_back(ConstantFP::get(Ctx, APFloat::getZero(Sem)));
    Cs.push_back(ConstantFP::get(Ctx, APFloat::getLargest(Sem)));
    Cs.push_back(ConstantFP::get(Ctx, APFloat::getSmallest(Sem)));
    break;
  }
  case Type::PointerTyID: {
    Cs.push_back(UndefValue::get(Ty));
    Cs.push_back(ConstantPointerNull::get(cast<PointerType>(Ty)));
    break;
  }
  case Type::StructTyID:
  case Type::ArrayTyID:
  case Type::FixedVectorTyID:
  case Type::ScalableVectorTyID:
    Cs.push_back(ConstantAggregateZero::get(Ty));
    break;
  case Type::TokenTyID:
    Cs.push_back(ConstantTokenNone::get(Ty->getContext()));
    break;
  default:
    llvm_unreachable("Cannot create a constant of that type!");
  }

  RS.sample(Cs);
  return RS.getSelection();
}

std::unique_ptr<Module> FunctionCombiner::cloneDestM() {
  assert(DestM);
  return CloneModule(*DestM);
}

std::unique_ptr<Module> FunctionCombiner::cloneSrcM() {
  assert(SrcM);
  return CloneModule(*SrcM);
}

void FunctionCombiner::deleteGlobalVal(GlobalValue *GV) {
  // TODO Is there a better way to avoid this casting bug?
  // i.e. we pass in a function to replace the first operand of the blockaddr
  SmallVector<BlockAddress *, 2> ToDelete;
  for (User *U : GV->users()) {
    if (BlockAddress *BA = dyn_cast<BlockAddress>(U)) {
      ToDelete.push_back(BA);
    }
  }
  for (auto *BA : ToDelete) {
    BA->replaceAllUsesWith(UndefValue::get(BA->getType()));
    assert(BA->hasZeroLiveUses());
    BA->destroyConstant();
  }
  GV->replaceAllUsesWith(UndefValue::get(GV->getType()));
  GV->eraseFromParent();
}

void FunctionCombiner::getEnclosingFns(Value *V,
                                       SmallSet<Function *, 4> &Fns) {
  if (Instruction *Inst = dyn_cast<Instruction>(V)) {
    Fns.insert(Inst->getFunction());
    return;
  }
  if (Constant *Const = dyn_cast<Constant>(V)) {
    for (User *ConstU : Const->users()) {
      Value *UserVal = dyn_cast<Value>(ConstU);
      getEnclosingFns(UserVal, Fns);
    }
  }
}

void FunctionCombiner::cleanModule(Module &M, Function *KeepF) {
  DEBUG_PRINT(dbgs() << "FUNC: "
                     << "FunctionCombiner::cleanModule\n";)

  SmallSet<Function *, 2> KeepFuncs;
  KeepFuncs.insert(KeepF);
  if (KeepF->hasPersonalityFn()) {
    Constant *ConstPFunc = KeepF->getPersonalityFn()->stripPointerCasts();
    if(Function *PFunc = dyn_cast<Function>(ConstPFunc)) {
      assert(PFunc && "Personality function is not a Function type!");
      KeepFuncs.insert(PFunc);
    }
  }

  SmallVector<GlobalValue *, 8> GVsToDelete;

  // Delete all unnecessary functions
  for (Function &F : M) {
    if (KeepFuncs.contains(&F) || F.isIntrinsic()) {
      continue;
    }

    // Don't delete any function calls used in the second arg of the
    // trampoline intrinsic
    bool SkipFunc = false;
    for (User *U : F.users()) {
      if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(U)) {
        if (Function *IF = II->getCalledFunction()) {
          if (Intrinsic::ID ID = (Intrinsic::ID)IF->getIntrinsicID()) {
            if (ID == Intrinsic::init_trampoline) {
              Value *Val = II->getArgOperand(1)->stripPointerCasts();
              Function *FVal = dyn_cast<Function>(Val);
              assert(FVal);
              if (FVal == &F) {
                 SkipFunc = true;
              }
            }
          }
        }
      }
    }

    // Don't delete functions with a speculatable attribute
    FunctionType *FT = F.getFunctionType();
    AttributeList Attrs = F.getAttributes();
    if (Attrs.hasFnAttr(Attribute::Speculatable)) {
      SkipFunc = true;
    }

    // Don't delete functions that take a swifterror argument
    for (unsigned I = 0, E = FT->getNumParams(); I != E; ++I) {
      AttributeSet ArgAttrs = Attrs.getParamAttrs(I);
      if (ArgAttrs.hasAttribute(Attribute::SwiftError)) {
        SkipFunc = true;
      }
    }

    if (!SkipFunc)
      GVsToDelete.push_back(&F);
  }

  // Delete all unnecessary globals variables
  // i.e. globals not used in KeepF
  for (auto &GV : M.globals()) {
    bool UsedInKeepF = false;
    for (User *U : GV.users()) {
      Value *UserVal = dyn_cast<Value>(U);
      SmallSet<Function *, 4> EnclosingFns;
      getEnclosingFns(UserVal, EnclosingFns);
      for (Function *EncF : EnclosingFns) {
        if (KeepFuncs.contains(EncF)) {
           UsedInKeepF = true;
        }
      }
    }
    if (!UsedInKeepF) {
      GVsToDelete.push_back(&GV);
    }
  }

  for (GlobalValue *GVal : GVsToDelete) {
    deleteGlobalVal(GVal);
  }
}

void FunctionCombiner::resolveGlobalSymbols() {
  DEBUG_PRINT(dbgs() << "FUNC: "
                     << "FunctionCombiner::resolveGlobalSymbols\n";)
  if (!SrcM.get() || SrcM.get() == DestM.get())
    return;

  SmallSet<std::string, 8> Symbols;

  auto GetUniqueSymbol = [&](std::string &S) -> void {
    std::string NewS = S;
    unsigned I = 0;
    while (Symbols.contains(NewS)) {
      NewS = S + std::to_string(I);
      I++;
    }
    Symbols.insert(NewS);
    S = NewS;
  };

  // Collect all existing symbols
  for (Function &F : *DestM.get()) {
    Symbols.insert(F.getName().str());
  }
  for (GlobalVariable &GV : DestM->globals()) {
    Symbols.insert(GV.getName().str());
  }
  for (GlobalAlias &GA : DestM->aliases()) {
    Symbols.insert(GA.getName().str());
  }
  for (GlobalIFunc &GI : DestM->ifuncs()) {
    Symbols.insert(GI.getName().str());
  }
  for (Function &F : *SrcM.get()) {
    Symbols.insert(F.getName().str());
  }
  for (GlobalVariable &GV : SrcM->globals()) {
    Symbols.insert(GV.getName().str());
  }
  for (GlobalAlias &GA : SrcM->aliases()) {
    Symbols.insert(GA.getName().str());
  }
  for (GlobalIFunc &GI : SrcM->ifuncs()) {
    Symbols.insert(GI.getName().str());
  }

  // Rename symbols in the source module
  for (Function &F : *SrcM.get()) {
    // Don't mess up intrinsic names or anonymous functions
    if (F.isIntrinsic() || !F.hasName())
      continue;

    std::string CurFName = F.getName().str() + ".SM";
    GetUniqueSymbol(CurFName);
    F.setName(CurFName);
  }
  for (GlobalVariable &GV : SrcM->globals()) {
    std::string CurGVName = GV.getName().str() + ".SM";
    GetUniqueSymbol(CurGVName);
    GV.setName(CurGVName);
  }
  for (GlobalAlias &GA : SrcM->aliases()) {
    std::string CurGAName = GA.getName().str() + ".SM";
    GetUniqueSymbol(CurGAName);
    GA.setName(CurGAName);
  }
  for (GlobalIFunc &GI : SrcM->ifuncs()) {
    std::string CurGIName = GI.getName().str() + ".SM";
    GetUniqueSymbol(CurGIName);
    GI.setName(CurGIName);
  }
}

bool FunctionCombiner::linkFunctionInto(Function *Func, Function **FuncPtr,
                                        Module &CompositeM, Linker &L) {
  DEBUG_PRINT(dbgs() << "FUNC: "
                     << "FunctionCombiner::linkFunctionInto\n";)

  assert(Func->hasName());

  Module *FuncParent = Func->getParent();

  // Handle Metadata initialization
  initializeMetadata(*FuncParent);

  std::unique_ptr<Module> FuncMod;
  {
    if (FuncParent == DestM.get())
      FuncMod = cloneDestM();
    else if (FuncParent == SrcM.get())
      FuncMod = cloneSrcM();
    else
      llvm_unreachable("Function must come from either DestM or SrcM!");
  }
  // Function *KeepFunc = FuncMod->getFunction(Func->getName());
  // assert(KeepFunc && "No function with that name was found!");
  // cleanModule(*FuncMod, KeepFunc);

  // Make the datalayouts consistent. Randomly select the resultant layout
  // if both FuncMod and CompositeM have the same layout
  auto &FuncDL = FuncMod->getDataLayoutStr();
  auto &CompDL = CompositeM.getDataLayoutStr();
  if (!FuncDL.empty() && !CompDL.empty()) {
    auto RS = makeSampler<std::string>(RIRB.Rand);
    RS.sample(FuncDL, 1);
    RS.sample(CompDL, 1);
    CompositeM.setDataLayout(RS.getSelection());
    FuncMod->setDataLayout(RS.getSelection());
  } else if (!FuncDL.empty()) {
    CompositeM.setDataLayout(FuncDL);
  } else if (!CompDL.empty()) {
    FuncMod->setDataLayout(CompDL);
  }

  // Make the module level flags consistent if both modules have their own
  // flags. The linker will handle most cases but will call an
  // llvm_unreachable if certain flag behaviors mismatch. We handle those here
  NamedMDNode *FuncNMD = FuncMod->getModuleFlagsMetadata();
  NamedMDNode *CompNMD = CompositeM.getModuleFlagsMetadata();

  if (FuncNMD && CompNMD) {

    // Create a map for our composite module flags and record any requirements
    DenseMap<MDString *, MDNode *> CompFlags;
    SmallSetVector<MDNode *, 16> Requirements;
    for (unsigned I = 0, E = CompNMD->getNumOperands(); I != E; ++I) {
      MDNode *Op = CompNMD->getOperand(I);
      auto *Behavior = mdconst::extract<ConstantInt>(Op->getOperand(0));
      MDString *ID = cast<MDString>(Op->getOperand(1));

      if (Behavior->getZExtValue() == Module::Require) {
        Requirements.insert(cast<MDNode>(Op->getOperand(2)));
      } else {
        CompFlags[ID] = Op;
      }
    }

    // Iterate over all flags in the function module and make the flag
    // behaviors consistent
    for (unsigned I = 0, E = FuncNMD->getNumOperands(); I != E; ++I) {
      MDNode *FuncOp = FuncNMD->getOperand(I);
      ConstantInt *FuncBehavior =
          mdconst::extract<ConstantInt>(FuncOp->getOperand(0));
      MDString *ID = cast<MDString>(FuncOp->getOperand(1));
      MDNode *CompOp = CompFlags.lookup(ID);
      unsigned FuncBehaviorVal = FuncBehavior->getZExtValue();

      // If this is a requirement, add it and continue.
      if (FuncBehaviorVal == Module::Require) {
        Requirements.insert(cast<MDNode>(FuncOp->getOperand(2)));
        continue;
      }

      // If the Composite module does not contain this flag, then add it
      // to our map, but let the linker handle the actual import
      if (!CompOp) {
        CompFlags[ID] = FuncOp;
        continue;
      }

      ConstantInt *CompBehavior =
          mdconst::extract<ConstantInt>(CompOp->getOperand(0));
      unsigned CompBehaviorVal = CompBehavior->getZExtValue();

      auto ReplaceFuncFlagOp = [&](const unsigned I) {
        FuncOp->replaceOperandWith(I, CompOp->getOperand(I));
      };

      // Handle error when two flags have override behavior but
      // different values
      if (CompBehaviorVal == Module::Override &&
          FuncBehaviorVal == Module::Override &&
          FuncOp->getOperand(2) != CompOp->getOperand(2)) {
        ReplaceFuncFlagOp(2);
        continue;
      }

      // Handle error when two flags differing behavior but are not
      // a Min/Max & Warning pair
      if (FuncBehaviorVal != CompBehaviorVal) {
        bool MinAndWarn = (FuncBehaviorVal == Module::Min &&
                           CompBehaviorVal == Module::Warning) ||
                          (CompBehaviorVal == Module::Min &&
                           FuncBehaviorVal == Module::Warning);
        bool MaxAndWarn = (FuncBehaviorVal == Module::Max &&
                           CompBehaviorVal == Module::Warning) ||
                          (CompBehaviorVal == Module::Max &&
                           FuncBehaviorVal == Module::Warning);
        if (!(MaxAndWarn || MinAndWarn))
          ReplaceFuncFlagOp(0);
      }

      // Handle Error behavior when values mismatch
      if ((CompBehaviorVal == Module::Error ||
           FuncBehaviorVal == Module::Error) &&
          FuncOp->getOperand(2) != CompOp->getOperand(2)) {
        ReplaceFuncFlagOp(2);
        continue;
      }
    }

    // Amend any requirement conflicts.
    for (unsigned I = 0, E = Requirements.size(); I != E; ++I) {
      MDNode *Requirement = Requirements[I];
      MDString *Flag = cast<MDString>(Requirement->getOperand(0));
      Metadata *ReqVal = Requirement->getOperand(1);
      MDNode *Op = CompFlags[Flag];

      // Add the requirement if the ID doesn't exist, otherwise update
      // the value if it doesn't match the requirement
      if (!Op) {
        CompositeM.addModuleFlag(Module::Error, Flag->getString(), ReqVal);
        continue;
      }
      if (Op->getOperand(2) != ReqVal)
        Op->replaceOperandWith(2, ReqVal);
    }
  }

  assert(!verifyModule(*FuncMod.get(), &dbgs()));

  bool Err = L.linkInModule(std::move(FuncMod));

  if (Err) {
    DEBUG_PRINT(dbgs() << "*** Linker failed to link modules! ***\n";)
    return false;
  }

  *FuncPtr = CompositeM.getFunction(Func->getName());
  if (!*FuncPtr) {
    DEBUG_PRINT(dbgs() << "No function with that name was found! This can"
                       << " occur if the linker optimizes out duplicate"
                       << " functions with different names\n";)
    return false;
  }
  return true;
}

void FunctionCombiner::initializeMetadata(Module &M) {
  for (Function &F : M) {
    NamedMDNode *FMD  = M.getNamedMetadata(F.getName());
    if (!FMD) {
      MDNode *NewMDNode = MDNode::get(Context,
                                      MDString::get(Context,
                                                    M.getSourceFileName()));
      FMD = M.getOrInsertNamedMetadata(F.getName());
      FMD->addOperand(NewMDNode);
    }
  }
}

void FunctionCombiner::insertCrossoverMetadata(Module &M,
                                               Function *F1,
                                               Function *F2,
                                               Function *NewF,
                                               FunctionCombiner::CrossTy T) {
  Module *M1 = F1->getParent();
  Module *M2 = F2->getParent();
  NamedMDNode *F1MD  = M1->getOrInsertNamedMetadata(F1->getName());
  NamedMDNode *F2MD  = M2->getOrInsertNamedMetadata(F2->getName());
  NamedMDNode *NewMD = M.getOrInsertNamedMetadata(NewF->getName());

  std::vector<Metadata *> F1MDVec;
  for (Metadata *MN : F1MD->operands())
    F1MDVec.push_back(MN);
  MDTuple *F1MDTuple = MDNode::get(Context, F1MDVec);

  std::vector<Metadata *> F2MDVec;
  for (Metadata *MN : F2MD->operands())
    F2MDVec.push_back(MN);
  MDTuple *F2MDTuple = MDNode::get(Context, F2MDVec);

  Metadata *NewMDArr[3];

  switch(T) {
  case Inline:
    NewMDArr[0] = MDString::get(Context, "Inline");
    break;
  case Sequence:
    NewMDArr[0] = MDString::get(Context, "Sequence");
    break;
  }
  NewMDArr[1] = F1MDTuple;
  NewMDArr[2] = F2MDTuple;

  MDTuple *NewMDTuple = MDTuple::get(Context, NewMDArr);
  NewMD->addOperand(NewMDTuple);
}

void FunctionInliner::getArgCandidates(Instruction *Inst,
                                        TypeToValuesMap &Vals) {

  DEBUG_PRINT(dbgs() << "FUNC: "
                     << "FunctionCombiner::getArgCandidates\n";)
  DEBUG_PRINT(dbgs() << "    Instr: " << *Inst << "\n";)

  // First collect all Function arguments as useable candidates
  Function *Func = Inst->getFunction();
  for (Argument &Arg : Func->args()) {
    DEBUG_PRINT(dbgs() << "    Val: " << Arg << "\n";)
    Vals[Arg.getType()].insert(&Arg);
  }

  // Next collect all instructions in basic blocks that dominate Inst
  SmallPtrSet<BasicBlock *, 8> Reachable;
  SmallVector<BasicBlock *, 8> Worklist;
  DominatorTree DT(*Func);
  BasicBlock *StopBB = &(Func->front());
  BasicBlock *CurBB = Inst->getParent();
  Worklist.push_back(CurBB);

  do {
    CurBB = Worklist.pop_back_val();
    Reachable.insert(CurBB);

    if (CurBB != StopBB) {
      for (BasicBlock *BB : predecessors(CurBB)) {
        const Instruction *Terminator = BB->getTerminator();
        assert(Terminator && "Basic block is not well formed!");
        if (Reachable.find(BB) == Reachable.end()
            && DT.dominates(Terminator, Inst)) {
          Worklist.push_back(BB);
          Reachable.insert(BB);
        }
      }
    }

    for (auto &CurInst : *CurBB) {
      if (&CurInst == Inst) {
        break;
      }
      DEBUG_PRINT(dbgs() << "    Val: " << CurInst << "\n";)
      Vals[CurInst.getType()].insert(&CurInst);
    }
  } while (!Worklist.empty());
}

bool FunctionInliner::inlineFunction(
    Instruction *DestInst, Function *SrcFunc,
    SmallVector<unsigned, 4> &DestInstOperands, TypeToValuesMap &Vals) {

  DEBUG_PRINT(dbgs() << "FUNC: "
                     << "FunctionCombiner::inlineFunction\n";)

  Function *DestFunc = DestInst->getFunction();
  Module *CompositeM = DestFunc->getParent();
  assert(CompositeM == SrcFunc->getParent());

  ModulePassManager MPM;
  ModuleAnalysisManager MAM;
  PassBuilder PB;
  LoopAnalysisManager LAM;
  FunctionAnalysisManager FAM;
  CGSCCAnalysisManager CGAM;

  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  MPM.run(*CompositeM, MAM);

  auto GetAssumptionCache = [&](Function &F) -> AssumptionCache & {
    return FAM.getResult<AssumptionAnalysis>(F);
  };
  auto &PSI = MAM.getResult<ProfileSummaryAnalysis>(*CompositeM);

  SmallVector<Value *, 4> ArgVals;

  // For each argument we need, randomly select an available variable
  // that has the same type
  for (Argument &Arg : SrcFunc->args()) {
    Type *ArgType = Arg.getType();
    assert(Vals.find(ArgType) != Vals.end());
    auto &Candidates = Vals[ArgType];
    int RandSteps = genRandNum(0, Candidates.size() - 1);
    auto RandIter = std::next(std::begin(Candidates), RandSteps);
    ArgVals.push_back(*RandIter);
  }

  // If we have more than one operand in the destination instruction that
  // we can replace, then select one randomly 
  unsigned DestInstOpIdx = DestInstOperands.front();
  if (DestInstOperands.size() > 1) {
    int RandIdx = genRandNum(0, DestInstOperands.size() - 1);
    DestInstOpIdx = DestInstOperands[RandIdx];
  }

  // Find a place to insert a call instruction to the SrcFunc
  Twine CallInstName = "call." + SrcFunc->getName();
  CallInst *SrcFuncCI = nullptr;
  {
    // Since PHINodes must be grouped at the top of their parent basic block,
    // Create a new BB and insert it into the edge between the predecessor
    // incoming block and the PHINode block. Then insert the function call
    // in this block.
    if (PHINode *PHI = dyn_cast<PHINode>(DestInst)) {
      BasicBlock *PredBB = PHI->getIncomingBlock(DestInstOpIdx);
      BasicBlock *PHIParentBB = DestInst->getParent();

      // SplitEdge will handle creation of the new BB and required branch
      // changes
      BasicBlock *NewBB = SplitEdge(PredBB, PHIParentBB, nullptr, nullptr,
                                    nullptr, "inline.insert.bb");

      if (!NewBB) {
        DEBUG_PRINT(dbgs() << "ERROR: SplitEdge failed!\n";)
        return false;
      }

      // Now that we have a new BB in between the previous BBs, insert our
      // call to the SrcFunc
      BasicBlock::iterator IP = NewBB->getFirstInsertionPt();
      IRBuilder<> IRB(&(*IP));
      SrcFuncCI = IRB.CreateCall(SrcFunc, ArgVals, CallInstName);
    }

    // Otherwise, simply insert a call instruction right before the
    // destination instruction with our arguments, then replace the
    // destination instruction operand with the call
    else {
      SrcFuncCI = CallInst::Create(SrcFunc, ArgVals, CallInstName, DestInst);
    }
  }

  assert(SrcFuncCI);
  DestInst->setOperand(DestInstOpIdx, SrcFuncCI);

  // Finally inline the function
  Function *Caller = SrcFuncCI->getCaller();
  InlineFunctionInfo IFI(
      /*cg=*/nullptr, GetAssumptionCache, &PSI,
      &FAM.getResult<BlockFrequencyAnalysis>(*Caller),
      &FAM.getResult<BlockFrequencyAnalysis>(*SrcFunc));
  InlineResult Res = InlineFunction(
      *SrcFuncCI, IFI, false, &FAM.getResult<AAManager>(*SrcFunc), false);

  if (!Res.isSuccess()) {
    DEBUG_PRINT(dbgs() << "ERROR: Call to InlineFunction failed!\n";)
    return false;
  }

  if (verifyModule(*CompositeM, &dbgs())) {
    DEBUG_PRINT(dbgs() << "ERROR: Function does not pass verification!\n";)
    return false;
  }

#ifdef LOG_CSV_STATS
  writeToCSV(SrcFunc, DestInst);
#endif
  return true;
}

std::unique_ptr<Module> FunctionInliner::inlineFunctions() {

  assert(DestFuncs.size() > 0);
  assert(SrcFuncs.size() > 0);

  // Attempt to cross over functions by inserting a function call to SrcFunc
  // by replacing an operand variable in the DstFunc
  auto TryInlining = [&](Function *DstFunc, Function *SrcFunc) -> bool {
    assert(DstFunc->getParent() == SrcFunc->getParent());

    // Make sure that the SrcFunc return type is not void
    Type *SrcRetTy = SrcFunc->getReturnType();
    if (SrcRetTy == Type::getVoidTy(Context)) {
      DEBUG_PRINT(dbgs() << "*** SrcFunc has void return type,"
                         << " skipping ***\n\n";)
      return false;
    }

    // Collect all insertion location candidates i.e. all instruction operands
    // that match the return type of the function that we will insert. This
    // insertion will replace the operand with the result of the inlined
    // function (or a code fragment).
    std::unordered_map<Instruction *, SmallVector<unsigned, 4>> DstOperands;
    for (auto &BB : *DstFunc) {
      for (auto &Inst : BB) {
        unsigned OpIdx = 0;
        for (Use &U : Inst.operands()) {
          if (Value *V = dyn_cast<Value>(U)) {
            if (isa<Constant>(V) || isa<Instruction>(V)) {
              if (V->getType() == SrcRetTy) {
                DstOperands[&Inst].push_back(OpIdx);
              }
            }
          }
          OpIdx++;
        }
      }
    }

    // Check that we have an operand that matches the return type of the
    // source function (i.e. the function we will inline)
    if (DstOperands.empty()) {
      DEBUG_PRINT(dbgs() << "*** No matching operand in DstFunc"
                         << " for SrcFunc return type,"
                         << " skipping ***\n\n";)
      return false;
    }

    // Append the attribute list from the source function to the destination
    // function
    for (const auto &Attr : SrcFunc->getAttributes().getFnAttrs()) {
      DstFunc->addFnAttr(Attr);
    }

    SmallVector<Instruction *, 4> ShuffledDstOperands;
    ShuffledDstOperands.reserve(DstOperands.size());
    for (auto Pair : DstOperands) {
      ShuffledDstOperands.push_back(Pair.first);
    }
    shuffleVector<Instruction *>(ShuffledDstOperands);

    // find a random operand in the destination function to replace with a
    // function call
    for (Instruction *Inst : ShuffledDstOperands) {

      // Skip any GC projection intrinsics
      if (isa<GCProjectionInst>(Inst)) {
        continue;
      }

      // Get all values before the destination operand that are available for
      // use as an argument to the source function
      bool CanInlineFunc = true;
      std::unordered_map<Type *, std::unordered_set<Value *>> ArgCandidates;
      getArgCandidates(Inst, ArgCandidates);

      if (ArgCandidates.size() == 0)
        continue;

      // Check we can match up the arguments we need for the call to SrcFunc
      // with values defined at the point of the current instruction
      for (Argument &Arg : SrcFunc->args()) {
        Type *ArgType = Arg.getType();
        CanInlineFunc &= (ArgCandidates.find(ArgType) != ArgCandidates.end());
      }

      // TODO: For now ignore GEPs since we can potentially introduce
      // out of bound accesses if we replace the GEP indices.
      // Only allow inlining into the ptr operand (i.e. operand 0)
      if (isa<GetElementPtrInst>(Inst)) {
        auto &OpIndices = DstOperands[Inst];
        CanInlineFunc &= (OpIndices.size() == 1 &&
                          OpIndices.front() == 0);
      }

      // We cannot inline the function into a landing pad operand since
      // the landing pad instruction must be the first non phi instr in
      // its block
      CanInlineFunc &= !isa<LandingPadInst>(Inst);

      // We cannot handle insertion into a PHI node if the parent basic
      // block is a landing pad. This is because we must split an edge
      // between the parent BB and one of its predecessors to legally
      // insert a value in the PHI and this will mess up the jump from
      // an invoke instruction to the landing pad
      CanInlineFunc &= !(isa<PHINode>(Inst) &&
                         Inst->getParent()->isLandingPad());

      if (!CanInlineFunc)
        continue;

      return inlineFunction(Inst, SrcFunc, DstOperands[Inst], ArgCandidates);
    }

    DEBUG_PRINT(dbgs() << "*** Could not find a legal insertion location"
                       << " for the SrcFunc, skipping ***\n\n";)
    return false;
  };

  // Rename globals to avoid linking errors due to symbols collisions
  resolveGlobalSymbols();

  // Try every combination of functions for a successful merge
  for (Function *F1 : DestFuncs) {
    for (Function *F2 : SrcFuncs) {
      if (F1 == F2) {
        continue;
      }

      // Skip all anonymous functions
      if (!F1->hasName() || !F2->hasName())
        continue;

      DEBUG_PRINT(dbgs() << "    Trying DstFunc: "
                         << F1->getName() << ", "
                         << "    SrcFunc: "
                         << F2->getName() << "\n";)

//      std::string ModName = "func-inline." + F1->getName().str()
//                                     + "." + F2->getName().str();
      std::string ModName = "inline-result";

      auto CompositeM = std::make_unique<Module>(ModName, Context);
      Function *F1Ret;
      Function *F2Ret;
      Linker L(*CompositeM);

      if (linkFunctionInto(F1, &F1Ret, *CompositeM, L) &&
          linkFunctionInto(F2, &F2Ret, *CompositeM, L) &&
          TryInlining(F1Ret, F2Ret)) {
        DEBUG_PRINT(dbgs() << "Cross succeeded!\n";)

        // Delete F2 (i.e. the SrcFunc) from composite module.
        // We do this to reduce seed corpus duplication, as the unmodified
        // F2 already exists in the component test case and does not need
        // to be kept again in the new resultant module.
        deleteGlobalVal(F2Ret);

        // Record crossover information as module level metadata.
        insertCrossoverMetadata(*CompositeM, F1, F2, F1Ret,
                                FunctionCombiner::Inline);

        return CompositeM;
      }
    }
  }

  DEBUG_PRINT(dbgs() << "Cross failed!\n";)
  return nullptr;
}

bool FunctionSequencer::isUnsupportedArgType(Type *Ty) {
  switch (Ty->getTypeID()) {
  case Type::X86_MMXTyID:
  case Type::X86_AMXTyID:
  case Type::TargetExtTyID:
  case Type::TypedPointerTyID:
    return true;
  case Type::VoidTyID:
  case Type::LabelTyID:
  case Type::MetadataTyID:
  case Type::FunctionTyID:
    llvm_unreachable("The type cannot be a function argument!");
  default:
    return false;
  }
}

Function *FunctionSequencer::SeqStrategy::createEmptyFunction(Module &Dest,
                                                              Type *RetTy) {
  DEBUG_PRINT(dbgs() << "FUNC: "
                     << "FunctionCombiner::createEmptyFunction\n";)
  LLVMContext &Ctx = Dest.getContext();
  FunctionType *FType = FunctionType::get(RetTy, {},
                                          /*isVarArg=*/false);
  Function *Caller = Function::Create(FType, GlobalValue::ExternalLinkage,
                                      "function_sequence_caller", &Dest);
  BasicBlock::Create(Ctx, "entryBB", Caller);
  return Caller;
}

void FunctionSequencer::SeqStrategy::getArgTypes(Function *F,
                                                 ArgTypeSet &Types) {
  for (Argument &Arg : F->args()) {
    Types.insert(Arg.getType());
  }
}

void FunctionSequencer::SeqStrategy::getRandConstArgs(Function *F,
                                                      ArgList &Args,
                                                      Value *ArgOverride) {
  for (Argument &Arg : F->args()) {
    Type *ArgType = Arg.getType();
    if (ArgOverride && ArgOverride->getType() == ArgType) {
      Args.push_back(ArgOverride);
    } else {
      Args.push_back(Parent.getRandConstant(ArgType));
    }
  }
}

bool FunctionSequencer::RetValSeq::trySequence(Function *F1, Function *F2,
                                               Function **CallerFunc,
                                               Value **CallerRetVal,
                                               Type **CallerRetValType,
                                               IRBuilder<> &IRB) {
  assert(F1->getParent() == F2->getParent());
  Module *ParentM = F1->getParent();
  Type *F1RetType = F1->getReturnType();
  Type *F2RetType = F2->getReturnType();

  if (F1RetType == Type::getVoidTy(Parent.Context) &&
      F2RetType == Type::getVoidTy(Parent.Context))
    return false;

  // We fill the following vectors with arguments that will be passed into
  // F1 and F2.
  ArgList FirstArgs;
  ArgList SecondArgs;
  ArgTypeSet F1ArgTypes;
  ArgTypeSet F2ArgTypes;
  getArgTypes(F1, F1ArgTypes);
  getArgTypes(F2, F2ArgTypes);

  auto ChainFuncReturnVals = [&](Function *FirstFunc,
                                 Function *SecondFunc) -> void {
    *CallerRetValType = SecondFunc->getReturnType();
    *CallerFunc = createEmptyFunction(*ParentM, *CallerRetValType);
    BasicBlock &EntryBB = (*CallerFunc)->getEntryBlock();
    IRB.SetInsertPoint(&EntryBB);

    // Populate FirstArgs, then create Call
    getRandConstArgs(FirstFunc, FirstArgs, nullptr);
    CallInst *FirstCall = IRB.CreateCall(FirstFunc, FirstArgs);

    // Populate SecondArgs, then create Call
    getRandConstArgs(SecondFunc, SecondArgs, FirstCall);
    CallInst *SecondCall = IRB.CreateCall(SecondFunc, SecondArgs);

    if (*CallerRetValType != Type::getVoidTy(Parent.Context))
      *CallerRetVal = SecondCall;
  };

  if (F2ArgTypes.find(F1RetType) != F2ArgTypes.end()) {
    DEBUG_PRINT(dbgs() << "*** F1 return type matches with an F2"
                       << " argument ***\n\n";)
    ChainFuncReturnVals(F1, F2);
    return true;
  }
  else if (F1ArgTypes.find(F2RetType) != F1ArgTypes.end()) {
    DEBUG_PRINT(dbgs() << "*** F2 return type matches with an F1"
                       << " argument ***\n\n";)
    ChainFuncReturnVals(F2, F1);
    return true;
  }

  return false;
}

bool FunctionSequencer::PtrArgSeq::trySequence(Function *F1, Function *F2,
                                               Function **CallerFunc,
                                               Value **CallerRetVal,
                                               Type **CallerRetValType,
                                               IRBuilder<> &IRB) {
  assert(F1->getParent() == F2->getParent());
  Module *ParentM = F1->getParent();
  Type *F1RetType = F1->getReturnType();

  // We fill the following vectors with arguments that will be passed into
  // F1 and F2.
  ArgList FirstArgs;
  ArgList SecondArgs;
  ArgTypeSet F1ArgTypes;
  ArgTypeSet F2ArgTypes;
  ArgTypeSet MatchingPtrTypes;
  getArgTypes(F1, F1ArgTypes);
  getArgTypes(F2, F2ArgTypes);

  for (Type *Iter : F1ArgTypes) {
    if (F2ArgTypes.find(Iter) != F2ArgTypes.end() && Iter->isPointerTy())
      MatchingPtrTypes.insert(Iter);
  }

  if (MatchingPtrTypes.size() == 0)
    return false;

  DEBUG_PRINT(dbgs() << "*** Both functions have a matching pointer arg"
                     << " ***\n\n";)

  auto ChainPtrArgVals = [&](Function *FirstFunc,
                             Function *SecondFunc) -> void {
    *CallerRetValType = SecondFunc->getReturnType();
    *CallerFunc = createEmptyFunction(*ParentM, *CallerRetValType);
    BasicBlock &EntryBB = (*CallerFunc)->getEntryBlock();
    IRB.SetInsertPoint(&EntryBB);

    // Select a random pointer type from our matching arguments
    auto RS = makeSampler<Type *>(Parent.RIRB.Rand);
    RS.sample(MatchingPtrTypes);
    Type *PtrTy = RS.getSelection();
    AllocaInst *AllocaA = IRB.CreateAlloca(PtrTy, nullptr, "A");

    getRandConstArgs(FirstFunc, FirstArgs, AllocaA);
    getRandConstArgs(SecondFunc, SecondArgs, AllocaA);
    IRB.CreateCall(FirstFunc, FirstArgs);
    CallInst *SecondCall = IRB.CreateCall(SecondFunc, SecondArgs);

    if (*CallerRetValType != Type::getVoidTy(Parent.Context))
      *CallerRetVal = SecondCall;
  };

  if (F1RetType == Type::getVoidTy(Parent.Context))
    ChainPtrArgVals(F1, F2);
  else
    ChainPtrArgVals(F2, F1);

  return true;
}

bool FunctionSequencer::BinOpSeq::trySequence(Function *F1, Function *F2,
                                              Function **CallerFunc,
                                              Value **CallerRetVal,
                                              Type **CallerRetValType,
                                              IRBuilder<> &IRB) {
  assert(F1->getParent() == F2->getParent());
  Module *ParentM = F1->getParent();
  Type *F1RetType = F1->getReturnType();
  Type *F2RetType = F2->getReturnType();

  auto CanCreateBinOp = [](Type *A, Type *B) -> bool {
    if (A != B)
      return false;

    return A->getScalarType()->isFloatingPointTy() ||
           A->getScalarType()->isIntegerTy();
  };

  if (!CanCreateBinOp(F1RetType, F2RetType))
    return false;

  DEBUG_PRINT(dbgs() << "*** The function return types are compatible with"
                        " with a binary op ***\n\n";)

  // We fill the following vectors with arguments that will be passed into
  // F1 and F2.
  ArgList FirstArgs;
  ArgList SecondArgs;
  *CallerRetValType = F1RetType;
  *CallerFunc = createEmptyFunction(*ParentM, *CallerRetValType);
  BasicBlock &EntryBB = (*CallerFunc)->getEntryBlock();
  IRB.SetInsertPoint(&EntryBB);
  getRandConstArgs(F1, FirstArgs, nullptr);
  getRandConstArgs(F2, SecondArgs, nullptr);
  CallInst *LHS = IRB.CreateCall(F1, FirstArgs);
  CallInst *RHS = IRB.CreateCall(F2, SecondArgs);

  bool IsFloat = F1RetType->getScalarType()->isFloatingPointTy();
  int R = Parent.genRandNum(0, IsFloat ? 6 : 12);
  Instruction::BinaryOps OpC;

  switch (R) {
  default: llvm_unreachable("Invalid BinOp");
  case 0:{OpC = (IsFloat?Instruction::FAdd : Instruction::Add); break; }
  case 1:{OpC = (IsFloat?Instruction::FSub : Instruction::Sub); break; }
  case 2:{OpC = (IsFloat?Instruction::FMul : Instruction::Mul); break; }
  case 3:{OpC = (IsFloat?Instruction::FDiv : Instruction::SDiv); break; }
  case 4:{OpC = (IsFloat?Instruction::FDiv : Instruction::UDiv); break; }
  case 5:{OpC = (IsFloat?Instruction::FRem : Instruction::SRem); break; }
  case 6:{OpC = (IsFloat?Instruction::FRem : Instruction::URem); break; }
  case 7: {OpC = Instruction::Shl;  break; }
  case 8: {OpC = Instruction::LShr; break; }
  case 9: {OpC = Instruction::AShr; break; }
  case 10:{OpC = Instruction::And;  break; }
  case 11:{OpC = Instruction::Or;   break; }
  case 12:{OpC = Instruction::Xor;  break; }
  }

  *CallerRetVal = IRB.CreateBinOp(OpC, LHS, RHS, "B");
  assert(*CallerRetValType == (*CallerRetVal)->getType());
  return true;
}


bool FunctionSequencer::VoidSeq::trySequence(Function *F1, Function *F2,
                                             Function **CallerFunc,
                                             Value **CallerRetVal,
                                             Type **CallerRetValType,
                                             IRBuilder<> &IRB) {
  (void) CallerRetValType;
  DEBUG_PRINT(dbgs() << "*** No data flow injection possible"
                     << " ***\n\n";)

  assert(F1->getParent() == F2->getParent());
  Module *ParentM = F1->getParent();

  // We fill the following vectors with arguments that will be passed into
  // F1 and F2.
  ArgList FirstArgs;
  ArgList SecondArgs;
  *CallerFunc = createEmptyFunction(*ParentM, *CallerRetValType);
  BasicBlock &EntryBB = (*CallerFunc)->getEntryBlock();
  IRB.SetInsertPoint(&EntryBB);
  getRandConstArgs(F1, FirstArgs, nullptr);
  getRandConstArgs(F2, SecondArgs, nullptr);
  IRB.CreateCall(F1, FirstArgs);
  IRB.CreateCall(F2, SecondArgs);

  return true;
}

std::unique_ptr<Module> FunctionSequencer::sequenceFunctions() {
  assert(DestFuncs.size() > 0);
  assert(SrcFuncs.size() > 0);

  auto TrySequencing = [&](Function *F1, Function *F2) -> bool {
    assert(F1->getParent() == F2->getParent());

    Module *ParentM = F1->getParent();
    bool unsupportedType = false;
    for (Argument &Arg : F1->args()) {
      unsupportedType |= isUnsupportedArgType(Arg.getType());
    }
    for (Argument &Arg : F2->args()) {
      unsupportedType |= isUnsupportedArgType(Arg.getType());
    }
    if (unsupportedType)
      return false;

    Function *CallerFunc = nullptr;
    Value *CallerRetVal = nullptr;
    Type *CallerRetValType = Type::getVoidTy(Context);
    IRBuilder<> IRB(Context);

    // Randomly select the order in which to try the sequencing mutations.
    // If they all fail, then default to the void sequencing strategy
    RetValSeq RVStrat(*this);
    PtrArgSeq PAStrat(*this);
    BinOpSeq  BOStrat(*this);
    SmallVector<SeqStrategy *, 4> Selector{ &RVStrat, &PAStrat, &BOStrat }
                                          ;
    shuffleVector<SeqStrategy *>(Selector);
    bool SequenceSuccess = false;

    for (auto *Strategy : Selector) {
      SequenceSuccess = Strategy->trySequence(F1, F2, &CallerFunc,
                                              &CallerRetVal,
                                              &CallerRetValType,
                                              IRB);
      if (SequenceSuccess)
        break;
    }

    if (!SequenceSuccess) {
      VoidSeq VSStrat(*this);
      VSStrat.trySequence(F1, F2, &CallerFunc, &CallerRetVal,
                          &CallerRetValType, IRB);
    }

    // Return the appropriate value or insert a void return
    assert(CallerFunc);
    assert(CallerFunc->getReturnType() == CallerRetValType);
    if (CallerRetVal) {
      IRB.CreateRet(CallerRetVal);
    } else {
      assert(Type::getVoidTy(Context) == CallerRetValType);
      IRB.CreateRetVoid();
    }

    if (verifyModule(*ParentM, &dbgs())) {
      DEBUG_PRINT(dbgs() << "ERROR: Function does not pass verification after"
                         << " sequencing!\n";)
      return false;
    }

    return true;
  };

  // Rename globals to avoid linking errors due to symbol collisions
  resolveGlobalSymbols();

  // Randomly select a function from F1 and a function from F2 to sequence.
  // The resultant module will be F1's module
  auto RS1 = makeSampler<Function *>(RIRB.Rand);
  auto RS2 = makeSampler<Function *>(RIRB.Rand);
  RS1.sample(DestFuncs);
  RS2.sample(SrcFuncs);
  Function *F1 = RS1.getSelection();
  Function *F2 = RS2.getSelection();

  // Skip all anonymous functions
  if (!F1->hasName() || !F2->hasName())
    return nullptr;

  DEBUG_PRINT(dbgs() << "    Trying F1: " << F1->getName() << ", "
                     << "    F2: " << F2->getName() << "\n";)

//  std::string ModName = "func-sequence." + F1->getName().str()
//                                 + "." + F2->getName().str();
  std::string ModName = "sequence-result";

  auto CompositeM = std::make_unique<Module>(ModName, Context);
  Function *F1Ret;
  Function *F2Ret;
  Linker L(*CompositeM);

  if (linkFunctionInto(F1, &F1Ret, *CompositeM, L) &&
      linkFunctionInto(F2, &F2Ret, *CompositeM, L) &&
      TrySequencing(F1Ret, F2Ret)) {

    Function *CallerFunc = CompositeM->getFunction("function_sequence_caller");
    // Record crossover information as module level metadata.
    insertCrossoverMetadata(*CompositeM, F1, F2, CallerFunc,
                            FunctionCombiner::Sequence);

    DEBUG_PRINT(dbgs() << "Cross succeeded!\n";)
    return CompositeM;
  }

  DEBUG_PRINT(dbgs() << "Cross failed!\n";)
  return nullptr;
}
