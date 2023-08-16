//===--- llvm-opt-fuzzer.cpp - Fuzzer for instruction selection ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Tool to fuzz optimization passes using libFuzzer.
//
//===----------------------------------------------------------------------===//

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/FuzzMutate/FuzzerCLI.h"
#include "llvm/FuzzMutate/IRCrossOver.h"
#include "llvm/FuzzMutate/IRMutator.h"
#include "llvm/IR/Verifier.h"
#include "llvm/InitializePasses.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

static codegen::RegisterCodeGenFlags CGF;

static cl::opt<std::string>
    TargetTripleStr("mtriple", cl::desc("Override target triple for module"));

// Passes to run for this fuzzer instance. Expects new pass manager syntax.
static cl::opt<std::string> PassPipeline(
    "passes",
    cl::desc("A textual description of the pass pipeline for testing"));
static cl::opt<bool> Verbose("v",
                             cl::desc("Prints debug info on action taken"));
static cl::opt<bool> NoInline("noinline",
                              cl::desc("Disables the inlining mutation"));
static cl::opt<bool> NoSequence("nosequence",
                                cl::desc("Disables the sequencing mutation"));

static std::unique_ptr<IRCrossOver> CrossOver;
static std::unique_ptr<IRMutator> Mutator;
static std::unique_ptr<TargetMachine> TM;

std::unique_ptr<IRCrossOver> createOptCrossOver() {
  std::vector<std::unique_ptr<IRCrossOverStrategy>> Strategies;
  if (!NoInline)
    Strategies.push_back(
      std::make_unique<FunctionInlineStrategy>());
  if (!NoSequence)
    Strategies.push_back(
      std::make_unique<FunctionSequencingStrategy>());
  return std::make_unique<IRCrossOver>(std::move(Strategies));
}

std::unique_ptr<IRMutator> createOptMutator() {
  std::vector<TypeGetter> Types{
      Type::getInt1Ty,  Type::getInt8Ty,  Type::getInt16Ty, Type::getInt32Ty,
      Type::getInt64Ty, Type::getFloatTy, Type::getDoubleTy};

  std::vector<std::unique_ptr<IRMutationStrategy>> Strategies;
  Strategies.push_back(
      std::make_unique<InjectorIRStrategy>(
          InjectorIRStrategy::getDefaultOps()));
  Strategies.push_back(
      std::make_unique<InstDeleterIRStrategy>());
  Strategies.push_back(std::make_unique<InstModificationIRStrategy>());

  return std::make_unique<IRMutator>(std::move(Types), std::move(Strategies));
}

extern "C" size_t LLVMFuzzerCustomCrossOver(const uint8_t *Data1, size_t Size1,
                                            const uint8_t *Data2, size_t Size2,
                                            uint8_t *Out, size_t MaxOutSize,
                                            unsigned int Seed) {
  assert(CrossOver &&
      "IR crossover should have been created during fuzzer initialization");

  if (Verbose)
    dbgs() << "CUSTOM CROSS OVER HIT\n";

  if (Size1 <= 1 || Size2 <= 1) {
    return 0;
  }

  LLVMContext Context;
  auto M1 = parseAndVerify(Data1, Size1, Context);
  auto M2 = parseAndVerify(Data2, Size2, Context);

  if (!M1) {
    errs() << "error: cross over input module M1 is broken!\n";
    return 0;
  }

   if (!M2) {
    errs() << "error: cross over input module M2 is broken!\n";
    return 0;
  }

  std::unique_ptr<Module> Composite =
      CrossOver->crossOverModules(std::move(M1), std::move(M2), Seed);

  if (!Composite) {
    errs() << "cross over failed to find a valid mutation\n";
    return 0;
  }

  if (verifyModule(*Composite, &errs())) {
    errs() << "cross over result doesn't pass verification\n";
#ifndef NDEBUG
    Composite->dump();
#endif
    // Avoid adding incorrect test cases to the corpus.
    return 0;
  }

  std::string Buf;
  {
    raw_string_ostream OS(Buf);
    WriteBitcodeToFile(*Composite, OS);
  }

  if (Buf.size() > MaxOutSize) {
    errs() << "cross over result exceeded max size\n";
    return 0;
  }

  auto NewM = parseAndVerify(
      reinterpret_cast<const uint8_t*>(Buf.data()), Buf.size(), Context);

  if (!NewM) {
    errs() << "cross over failed to re-read the module\n";
#ifndef NDEBUG
    Composite->dump();
#endif
    return 0;
  }

  memcpy(Out, Buf.data(), Buf.size());
  return Buf.size();
}

extern "C" LLVM_ATTRIBUTE_USED size_t LLVMFuzzerCustomMutator(
    uint8_t *Data, size_t Size, size_t MaxSize, unsigned int Seed) {

  assert(Mutator &&
      "IR mutator should have been created during fuzzer initialization");

  if (Verbose)
    dbgs() << "CUSTOM MUTATOR HIT\n";

  LLVMContext Context;
  auto M = parseAndVerify(Data, Size, Context);
  if (!M) {
    errs() << "error: mutator input module is broken!\n";
    return 0;
  }

  Mutator->mutateModule(*M, Seed, Size, MaxSize);

  if (verifyModule(*M, &errs())) {
    errs() << "mutation result doesn't pass verification\n";
#ifndef NDEBUG
    M->dump();
#endif
    // Avoid adding incorrect test cases to the corpus.
    return 0;
  }
  
  std::string Buf;
  {
    raw_string_ostream OS(Buf);
    WriteBitcodeToFile(*M, OS);
  }
  if (Buf.size() > MaxSize)
    return 0;

  // There are some invariants which are not checked by the verifier in favor
  // of having them checked by the parser. They may be considered as bugs in the
  // verifier and should be fixed there. However until all of those are covered
  // we want to check for them explicitly. Otherwise we will add incorrect input
  // to the corpus and this is going to confuse the fuzzer which will start 
  // exploration of the bitcode reader error handling code.
  auto NewM = parseAndVerify(
      reinterpret_cast<const uint8_t*>(Buf.data()), Buf.size(), Context);
  if (!NewM) {
    errs() << "mutator failed to re-read the module\n";
#ifndef NDEBUG
    M->dump();
#endif
    return 0;
  }

  memcpy(Data, Buf.data(), Buf.size());
  return Buf.size();
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t Size) {
  assert(TM && "Should have been created during fuzzer initialization");

  if (Size <= 1)
    // We get bogus data given an empty corpus - ignore it.
    return 0;

  // Parse module
  //

  LLVMContext Context;
  auto M = parseAndVerify(Data, Size, Context);
  if (!M) {
    errs() << "error: input module is broken!\n";
    return 0;
  }

  // Set up target dependant options
  //

  M->setTargetTriple(TM->getTargetTriple().normalize());
  M->setDataLayout(TM->createDataLayout());
  codegen::setFunctionAttributes(TM->getTargetCPU(),
                                 TM->getTargetFeatureString(), *M);

  // Create pass pipeline
  //

  PassBuilder PB(TM.get());

  LoopAnalysisManager LAM;
  FunctionAnalysisManager FAM;
  CGSCCAnalysisManager CGAM;
  ModulePassManager MPM;
  ModuleAnalysisManager MAM;

  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  auto Err = PB.parsePassPipeline(MPM, PassPipeline);
  assert(!Err && "Should have been checked during fuzzer initialization");
  // Only fail with assert above, otherwise ignore the parsing error.
  consumeError(std::move(Err));

  // Run passes which we need to test
  //

  if (Verbose)
    dbgs() << "TESTING ONE INPUT ON THE PASS PIPELINE\n";

  MPM.run(*M, MAM);

  // Check that passes resulted in a correct code
  if (verifyModule(*M, &errs())) {
    errs() << "error: Transformation resulted in an invalid module\n";
    abort();
  }

  return 0;
}

static void handleLLVMFatalError(void *, const char *Message, bool) {
  // TODO: Would it be better to call into the fuzzer internals directly?
  dbgs() << "LLVM ERROR: " << Message << "\n"
         << "Aborting to trigger fuzzer exit handling.\n";
  abort();
}

extern "C" LLVM_ATTRIBUTE_USED int LLVMFuzzerInitialize(
    int *argc, char ***argv) {

  // Parse input options
  //

  handleExecNameEncodedOptimizerOpts(*argv[0]);
  parseFuzzerCLOpts(*argc, *argv);

  if (NoInline && NoSequence) {
    dbgs() << "Cannot disable both the inlining and sequencing mutations!\n";
    abort();
  }

  if (Verbose)
    dbgs() << "INITIALIZING FUZZER...\n";

  EnableDebugBuffering = true;

  // Make sure we print the summary and the current unit when LLVM errors out.
  install_fatal_error_handler(handleLLVMFatalError, nullptr);

  // Initialize llvm
  //

  InitializeAllTargets();
  InitializeAllTargetMCs();

  PassRegistry &Registry = *PassRegistry::getPassRegistry();
  initializeCore(Registry);
  initializeScalarOpts(Registry);
  initializeVectorization(Registry);
  initializeIPO(Registry);
  initializeAnalysis(Registry);
  initializeTransformUtils(Registry);
  initializeInstCombine(Registry);
  initializeTarget(Registry);

  // Create TargetMachine
  //

  if (TargetTripleStr.empty()) {
    errs() << *argv[0] << ": -mtriple must be specified\n";
    exit(1);
  }
  Triple TargetTriple = Triple(Triple::normalize(TargetTripleStr));

  std::string Error;
  const Target *TheTarget =
      TargetRegistry::lookupTarget(codegen::getMArch(), TargetTriple, Error);
  if (!TheTarget) {
    errs() << *argv[0] << ": " << Error;
    exit(1);
  }

  TargetOptions Options =
      codegen::InitTargetOptionsFromCodeGenFlags(TargetTriple);
  TM.reset(TheTarget->createTargetMachine(
      TargetTriple.getTriple(), codegen::getCPUStr(), codegen::getFeaturesStr(),
      Options, codegen::getExplicitRelocModel(),
      codegen::getExplicitCodeModel(), CodeGenOpt::Default));
  assert(TM && "Could not allocate target machine!");

  // Check that pass pipeline is specified and correct
  //

  if (PassPipeline.empty()) {
    errs() << *argv[0] << ": at least one pass should be specified\n";
    exit(1);
  }

  PassBuilder PB(TM.get());
  ModulePassManager MPM;
  if (auto Err = PB.parsePassPipeline(MPM, PassPipeline)) {
    errs() << *argv[0] << ": " << toString(std::move(Err)) << "\n";
    exit(1);
  }

  // Create mutator
  //

  Mutator = createOptMutator();
  CrossOver = createOptCrossOver();

  return 0;
}
