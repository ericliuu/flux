'''

This scripts runs the llvm-opt-fuzzer continuously in a loop and logs
the results

Arguments
-  fuzzer_binary         Location of the llvm-opt-fuzzer binary (release build)
-  opt_debug             Location of the opt binary (debug build for crash triage)
-  crashes_dir           Location of all existing crashes (for crash dedup)
-  output_corpus         Location of the output directory for fuzzer generated tests
-  seeds                 Location of the seed corpus
-  arch                  Target architecture
-  opt_passes            Optimization passes to fuzz
-  bug_output_dir        Directory that found bugs will be moved into

'''

import argparse
import os
import random
import subprocess
import time

class CrashLogger:
  def __init__(self, crashes_dir, opt, arch, passes):
    self.crashes_dir = crashes_dir
    self.opt = opt
    self.arch = arch
    self.passes = passes
    self.crashes = {}
    self.__loadExistingCrashHashes()

  def triageNewTest(self, file_name, file_path) -> bool:
    assert(os.path.exists(file_path))
    return self.__runOpt(file_name, file_path)

  def __addHashToCrashes(self, result, file_name) -> bool:
    
    # many of the stack dumps result in the same assertion error, hence
    # drop the stack dump before hashing
    if "Stack dump:" in result:
      drop_stack_dump = result.find("Stack dump:")
      result = result[:drop_stack_dump]

    hash_val = hash(result)
    if hash_val in self.crashes:
      print("Duplicate crash found! Test cases:")
      print("    {}".format(self.crashes[hash_val]))
      print("    {}".format(file_name))   
      return False

    self.crashes[hash_val] = file_name
    return True

  def __runOpt(self, file_name, file_path) -> bool:
    result = subprocess.run(
      [self.opt, "-passes=" + self.passes, "-mtriple", self.arch, file_path],
      capture_output = True,
      text = True,
      encoding = "ISO-8859-1"
    )
    if result.stderr == b'':
      print("No crash found for test {}!".format(file_name))
      return False;
    else:
      return self.__addHashToCrashes(result.stderr, file_name)

  def __loadExistingCrashHashes(self):
    for entry in os.scandir(self.crashes_dir):
      if entry.is_dir():
        for file in os.scandir(entry):
          file_name = os.path.basename(file)
          if file.is_file():
            if file_name.startswith("crash-"):
              print("Getting hash for {}".format(file_name))
              self.__runOpt(file_name, file.path)

def main():
  # get command line arguments
  parser = argparse.ArgumentParser(
                    prog = 'fuzzer_runner',
                    description = 'Runs the llvm-opt-fuzzer binary continuously \
                                   while triaging bugs against previous findings')
  parser.add_argument('fuzzer_binary',
                      help = 'Location of the llvm-opt-fuzzer binary (release build)')
  parser.add_argument('opt_debug',
                      help = 'Location of the opt binary (debug build for crash triage)')
  parser.add_argument('crashes_dir',
                      help = 'Location of all existing crashes (for crash dedup)')
  parser.add_argument('output_corpus',
                      help = 'Location of the output directory for fuzzer generated tests')
  parser.add_argument('bug_output_dir', help = 'Directory that found bugs will be moved into')
  parser.add_argument('seeds',
                      help = 'Location of the seed corpus')
  parser.add_argument('arch', help = 'Target architecture')
  parser.add_argument('opt_passes', help = 'Optimization passes to fuzz')
  parser.add_argument('-c', '--cross_over', type=int,
                      help = 'Optional int arg that sets crossover mode, default 1',
                      default=1, choices=[0, 1])
  parser.add_argument('-e', '--extra_flag', type=str,
                      help = 'Optional arg that is used to disable inline (-noinline) or sequence (-nosequence)',
                      choices=['-noinline', '-nosequence'],)
  parser.add_argument('-s', '--same_corpus', action='store_true',
                      help='Use the same output corpus for each fuzzing iteration if specified')
  args = parser.parse_args()

  assert(os.path.exists(args.fuzzer_binary))
  assert(os.path.exists(args.opt_debug))
  assert(os.path.exists(args.crashes_dir))
  assert(os.path.exists(args.output_corpus))
  assert(os.path.exists(args.seeds))
  assert(os.path.exists(args.bug_output_dir))
  assert(args.cross_over == 0 or args.cross_over == 1)
  assert(not args.extra_flag or
         args.extra_flag == '-nosequence' or
         args.extra_flag == '-noinline')
  
  # collect all previous crashes and store them in a set of hash values
  crashLogger = CrashLogger(args.crashes_dir, args.opt_debug, args.arch, args.opt_passes)

  duplicate_dir = args.bug_output_dir + "/" + "duplicate/"
  new_bug_dir   = args.bug_output_dir + "/" + "new/"
  if not os.path.exists(duplicate_dir):
    os.mkdir(duplicate_dir)
  if not os.path.exists(new_bug_dir):
    os.mkdir(new_bug_dir)

  # llvm-opt-fuzzer running loop
  iteration = 0
  while True:
    output_corpus = args.output_corpus + "/fuzzing-run/"
    seed = random.randint(0, 777777777)
    if not args.same_corpus:
      output_corpus = args.output_corpus + "/fuzzing-run-" + str(iteration) + \
                      "-seed-" + str(seed) + "/"
    if not os.path.exists(output_corpus):
      os.mkdir(output_corpus)
    fuzzer_flags = [
      args.fuzzer_binary,
      "-seed=" + str(seed),
      "-rss_limit_mb=3072",
      output_corpus,
      args.seeds,
      "-cross_over_uniform_dist=1",
      "-cross_over=" + str(args.cross_over),
      "-max_len=8000",
      "-keep_seed=1",
      "-ignore_remaining_args=1",
      "-mtriple", args.arch,
      "-passes=" + args.opt_passes,
    ]
    if args.extra_flag:
      fuzzer_flags += [ args.extra_flag ]

    start_time = time.time()
    print("=== Starting Fuzzing Iteration {} ===".format(iteration))
    result = subprocess.run(
      fuzzer_flags,
      capture_output = True,
      text = True,
      encoding="ISO-8859-1"
    )
    print("=== Finished Fuzzing Iteration {} in {} minutes ===".format(
      iteration,
      (time.time() - start_time) / 60.0
    ))
    print("=== Dumping Fuzzing Iteration {} StdErr ===".format(iteration))
    print(result.stderr)
    iteration += 1

    # extract file name of the crash test
    stderr = result.stderr
    target_str = "Test unit written to "
    assert(target_str in result.stderr), result.stderr
    crash_file_idx = stderr.find(target_str)
    crash_file_idx += len(target_str)
    stderr = stderr[crash_file_idx:]
    crash_file_end_idx = stderr.find(" ")
    crash_file = stderr[:crash_file_end_idx]
    assert(crash_file[0] == ".")
    assert(crash_file[1] == "/")
    crash_file = crash_file[2:]

    crash_filepath = os.getcwd() + "/" + crash_file
    if not os.path.exists(crash_filepath):
      print("PATH DOES NOT EXIST: {}".format(crash_filepath))
      continue

    # move the output testcase to either the duplicate directory or new directory
    if crashLogger.triageNewTest(crash_file, crash_filepath):
      os.rename(crash_filepath, new_bug_dir + crash_file)
    else:
      os.rename(crash_filepath, duplicate_dir + crash_file)

if __name__ == "__main__":
  main()
