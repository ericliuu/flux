#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/FuzzMutate/IRCrossOver.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Object/Archive.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"

using namespace llvm;

static cl::OptionCategory CrossCategory("Crossover Options");

static cl::list<std::string> InputFilenames(cl::Positional, cl::OneOrMore,
                                            cl::desc("<input bitcode files>"),
                                            cl::cat(CrossCategory));

static cl::opt<std::string>
    OutputFilename("o", cl::desc("Override output filename"), cl::init("-"),
                   cl::value_desc("filename"), cl::cat(CrossCategory));

static cl::opt<bool> OutputAssembly("S",
                                    cl::desc("Write output as LLVM assembly"),
                                    cl::Hidden, cl::cat(CrossCategory));

static cl::opt<bool> Verbose("v",
                             cl::desc("Print information about actions taken"),
                             cl::cat(CrossCategory));

static cl::opt<bool> NoVerify("disable-verify",
                              cl::desc("Do not run the verifier"), cl::Hidden,
                              cl::cat(CrossCategory));

static cl::opt<unsigned> SeedCL("seed", cl::desc("Seed used for randomness"),
                                cl::init(0), cl::cat(CrossCategory));
static cl::opt<bool> NoInline("noinline",
                              cl::desc("Disables the inlining mutation"),
                              cl::cat(CrossCategory));
static cl::opt<bool> NoSequence("nosequence",
                                cl::desc("Disables the sequencing mutation"),
                                cl::cat(CrossCategory));

static ExitOnError ExitOnErr;

std::unique_ptr<IRCrossOver> createCrossOver() {
  std::vector<std::unique_ptr<IRCrossOverStrategy>> Strategies;
  if (!NoInline)
    Strategies.push_back(
      std::make_unique<FunctionInlineStrategy>());
  if (!NoSequence)
    Strategies.push_back(
      std::make_unique<FunctionSequencingStrategy>());
  return std::make_unique<IRCrossOver>(std::move(Strategies));
}

static std::unique_ptr<Module> loadFile(const char *argv0,
                                        std::unique_ptr<MemoryBuffer> Buffer,
                                        LLVMContext &Context) {
  SMDiagnostic Err;
  if (Verbose)
    errs() << "Loading '" << Buffer->getBufferIdentifier() << "'\n";
  std::unique_ptr<Module> Result = parseIR(*Buffer, Err, Context);

  if (!Result) {
    Err.print(argv0, errs());
    return nullptr;
  }

  return Result;
}

int main(int argc, char **argv) {
  InitLLVM X(argc, argv);
  ExitOnErr.setBanner(std::string(argv[0]) + ": ");

  std::unique_ptr<IRCrossOver> CrossOver = createCrossOver();
  LLVMContext Context;
  cl::ParseCommandLineOptions(argc, argv, "fuzzer crossover\n");

  if (NoInline && NoSequence) {
    dbgs() << "Cannot disable both the inlining and sequencing mutations!\n";
    return 0;
  }

  if (Verbose)
    errs() << "Reading in first two input files...\n";

  SmallVector<std::unique_ptr<Module>, 2> InputModules;
  for (const auto &File : InputFilenames) {
    if (InputModules.size() < 2) {
      std::unique_ptr<MemoryBuffer> Buffer =
          ExitOnErr(errorOrToExpected(MemoryBuffer::getFileOrSTDIN(File)));
      std::unique_ptr<Module> M = loadFile(argv[0], std::move(Buffer), 
                                           Context);
      InputModules.push_back(std::move(M));
    } else if (Verbose) {
      errs() << "File: " << File << " ignored\n";
    }
  }

  if (!NoVerify) {
    for (unsigned I = 0; I < InputModules.size(); ++I) {
      if (verifyModule(*InputModules[I].get(), &errs())) {
        errs() << "Error: input module " << I
               << " does not pass verification!\n";
        return 0;
      }
    }
  }
  
  std::unique_ptr<Module> Composite;
  if (InputModules.size() == 1) {
    if (Verbose)
      errs() << "Only one file! Proceeding with single module mutation...\n";
    Composite = CrossOver->crossOverFunctions(std::move(InputModules[0]),
                                              SeedCL);
  } else {
    if (Verbose)
      errs() << "Proceeding with module crossover mutation...\n";
    Composite = CrossOver->crossOverModules(std::move(InputModules[0]),
                                            std::move(InputModules[1]),
                                            SeedCL);
  }

  if (!Composite) {
    errs() << "Cross over failed to find a valid mutation!\n";
    return 0;
  }

  std::error_code EC;
  ToolOutputFile Out(OutputFilename, EC,
                     OutputAssembly ? sys::fs::OF_TextWithCRLF
                                    : sys::fs::OF_None);
  if (EC) {
    WithColor::error() << EC.message() << '\n';
    return 1;
  }

  if (verifyModule(*Composite, &errs())) {
    errs() << argv[0] << ": ";
    WithColor::error() << "Crossed module does not pass verification!\n";

    if (!NoVerify) {
      return 1;
    }

    WithColor::error() << "Continuing with output as -disable-verify was"
                       << "chosen...\n";
  }

  if (Verbose)
    errs() << "Writing bitcode...\n";
  if (OutputAssembly) {
    Composite->print(Out.os(), nullptr, true);
  } else if (!CheckBitcodeOutputToConsole(Out.os()))
    WriteBitcodeToFile(*Composite, Out.os(), true);

  // Declare success.
  Out.keep();

  return 0;
}
