#include "llvm/FuzzMutate/ComposeTests.h"
#include "llvm/FuzzMutate/IRCrossOver.h"
#include "llvm/FuzzMutate/IRMutator.h"
#include "llvm/FuzzMutate/Random.h"
#include "llvm/FuzzMutate/RandomIRBuilder.h"
#include "llvm/Analysis/AliasAnalysis.h"

#include <random>

// #define CROSSOVER_DEBUG

#ifdef CROSSOVER_DEBUG
#define CROSSOVER_DEBUG_PRINT(x) x
#else
#define CROSSOVER_DEBUG_PRINT(x)
#endif

using namespace llvm;

UniqModPtr IRCrossOver::crossOverModules(UniqModPtr M1, UniqModPtr M2,
                                         int Seed) {
  RandomIRBuilder IB(Seed, {});
  auto RS = makeSampler<IRCrossOverStrategy *>(IB.Rand);
  for (const auto &Strategy : Strategies) {
    RS.sample(Strategy.get(), 4);
  }
  auto Strategy = RS.getSelection();

  return Strategy->crossOver(std::move(M1), std::move(M2), Seed);
}

UniqModPtr IRCrossOver::crossOverFunctions(UniqModPtr M, int Seed) {
  RandomIRBuilder IB(Seed, {});
  auto RS = makeSampler<IRCrossOverStrategy *>(IB.Rand);
  for (const auto &Strategy : Strategies) {
    RS.sample(Strategy.get(), 4);
  }
  auto Strategy = RS.getSelection();

  return Strategy->crossOver(std::move(M), Seed);
}

UniqModPtr FunctionInlineStrategy::crossOver(UniqModPtr M1, UniqModPtr M2,
                                             int Seed) {
  CROSSOVER_DEBUG_PRINT(
    dbgs() << "FUNC: FunctionInlineStrategy::crossOver\n";)

  FunctionInliner FI(std::move(M1), std::move(M2), Seed);
  return FI.inlineFunctions();
}

UniqModPtr FunctionSequencingStrategy::crossOver(UniqModPtr M1, UniqModPtr M2,
                                                 int Seed) {
  CROSSOVER_DEBUG_PRINT(
    dbgs() << "FUNC: FunctionSequencingStrategy::crossOver\n";)

  FunctionSequencer FS(std::move(M1), std::move(M2), Seed);
  return FS.sequenceFunctions();
}

UniqModPtr FunctionInlineStrategy::crossOver(UniqModPtr M, int Seed) {
  CROSSOVER_DEBUG_PRINT(
    dbgs() << "FUNC: FunctionInlineStrategy::crossOver\n";)

  FunctionInliner FI(std::move(M), Seed);
  return FI.inlineFunctions();
}

UniqModPtr FunctionSequencingStrategy::crossOver(UniqModPtr M, int Seed) {
  CROSSOVER_DEBUG_PRINT(
    dbgs() << "FUNC: FunctionSequencingStrategy::crossOver\n";)

  FunctionSequencer FS(std::move(M), Seed);
  return FS.sequenceFunctions();
}
