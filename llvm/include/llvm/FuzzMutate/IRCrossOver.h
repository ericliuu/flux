#ifndef LLVM_FUZZMUTATE_IRCROSSOVER_H
#define LLVM_FUZZMUTATE_IRCROSSOVER_H

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/ErrorHandling.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <memory>

namespace llvm {

class Type;
class Instruction;
class Value;
class Function;
class Module;

using UniqModPtr = std::unique_ptr<Module>;

/// Base class for describing how to crossover two modules
class IRCrossOverStrategy {
public:
  virtual ~IRCrossOverStrategy() = default;

  virtual UniqModPtr crossOver(UniqModPtr M1, UniqModPtr M2, int Seed) {
    llvm_unreachable("Strategy does not implement any mutators");
  }

  virtual UniqModPtr crossOver(UniqModPtr M, int Seed) {
    llvm_unreachable("Strategy does not implement any mutators");
  }
};

/// Entry point for configuring and running IR crossovers.
class IRCrossOver {
  std::vector<std::unique_ptr<IRCrossOverStrategy>> Strategies;

public:
  IRCrossOver(std::vector<std::unique_ptr<IRCrossOverStrategy>> &&Strategies)
      : Strategies(std::move(Strategies)) {}

  UniqModPtr crossOverModules(UniqModPtr M1, UniqModPtr M2, int Seed);
  UniqModPtr crossOverFunctions(UniqModPtr M, int Seed);
};

class FunctionInlineStrategy : public IRCrossOverStrategy {
public:
  /// Crossover two modules by choosing one function from each module
  /// and replacing an operand in one function with an inlined call to
  /// the other function.
  /// Returns true if the operation succeeds, false otherwise.
  UniqModPtr crossOver(UniqModPtr M1, UniqModPtr M2, int Seed) override;

  /// Crossover two functions in a single module by replacing an operand
  /// in one function with an inlined call to the other function.
  UniqModPtr crossOver(UniqModPtr M, int Seed) override;
};

class FunctionSequencingStrategy : public IRCrossOverStrategy {
public:
  /// Crossover two modules by placing two functions from M1 and M2
  /// into a single module and create a third function that calls the
  /// two functions in sequence.
  /// Returns true if the operation succeeds, false otherwise.
  UniqModPtr crossOver(UniqModPtr M1, UniqModPtr M2, int Seed) override;

  /// Crossover two functions in a single module by placing the two functions
  /// into a single module and create a third function that calls the
  /// two functions in sequence.
  UniqModPtr crossOver(UniqModPtr M, int Seed) override;
};

} // end llvm namespace

#endif // LLVM_FUZZMUTATE_IRCROSSOVER_H
