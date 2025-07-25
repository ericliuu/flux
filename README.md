# FLUX-libFuzzer

FLUX (**F**inding bugs with **L**LVM IR based **U**nit test cross(**X**)overs) is a libFuzzer extension designed to find bugs in LLVM's optimization passes.
FLUX accomplishes this by randomly combining pairs of LLVM IR files from LLVM's existing unit test suite (i.e. `llvm/test/Transforms`).

FLUX is built on version LLVM 16.0 (Specifically commit `0303eafcb34647f7d5a4015aad266b5766f5dc5e`)

## Summary

FLUX's fuzzing loop repeatedly combines IR files that are supplied in the seed corpus.
In each iteration the resulting program is fed into LLVM's optimizer and checked for validity.
We build FLUX on top of LLVM's existing `llvm-opt-fuzzer` which itself is built on `libFuzzer`.

For a technical description of FLUX's crossovers, please refer to our [ASE2023 paper](https://ieeexplore.ieee.org/abstract/document/10298377)

## Collecting Unit Tests for Fuzzing

We provide a script that collects all LLVM transformation unit tests into a single directory which can be passed into libFuzzer as a corpus.
The script uses `llvm-extract` and `llvm-as` and an easy way to acquire these binaries is to build LLVM.
To use the script enter the following steps from the root of the git repo:

```bash
> cd FLUX
> mkdir build
> cd build
> cmake -G Ninja \
        -DCMAKE_BUILD_TYPE=Release \
        -DLLVM_USE_LINKER=lld \
        -DLLVM_ENABLE_PROJECTS="clang;compiler-rt" \
        ../llvm
> ninja
> cd ../../
> python3 scripts/collect_unittest_functions.py 
```

By default, the script reads from the unit tests located in the `FLUX` subdirectory, but this can be easily changed by specifying a different path in the above script.
The script will generate four subdirectories in the `unit-tests` directory. The `ll_{files,functions}` directories hold the original unit tests and the unit tests with each individual function extracted into its own file, respectively. The `bc_{files,functions}` directories hold the compiled results of their corresponding `.ll` files.

## Building FLUX

To build FLUX for fuzzing LLVM optimizations, we will need to compile LLVM with a version of `clang` and `clang++` that contain FLUX's changes on LLVM's compiler-rt libraries.
Luckily if you followed the build instructions in the "Collecting Unit Tests for Fuzzing" section, we will already have these binaries.
To build `llvm-opt-fuzzer` that is linked with the version of LLVM that FLUX is built on, run the following from the root of the git repo:

```bash
> cd FLUX
> mkdir build-fuzz
> cd build-fuzz
> CC=[Path to flux]/flux/FLUX/build/bin/clang \
  CXX=[Path to flux]/flux/FLUX/build/bin/clang++ \
  CFLAGS="-fsanitize-coverage=edge,inline-8bit-counters,no-prune \
          -fno-sanitize-coverage=trace-cmp,trace-div,bb,func,trace-pc-guard,trace-pc,indirect-calls,pc-table" \
  CXXFLAGS="-fsanitize-coverage=edge,inline-8bit-counters,no-prune \
          -fno-sanitize-coverage=trace-cmp,trace-div,bb,func,trace-pc-guard,trace-pc,indirect-calls,pc-table" \
  cmake -G Ninja \
        -DCMAKE_BUILD_TYPE=Release \
        -DLLVM_ENABLE_ASSERTIONS=On \
        -DLLVM_BUILD_RUNTIME=Off \
        -DLLVM_USE_SANITIZE_COVERAGE=On \
        -DLLVM_USE_LINKER=lld \
        ../llvm
> ninja llvm-opt-fuzzer
```

Note that we enable SanitizerCoverage's 8bit-counters as a loose proxy for path coverage.

To test other versions of LLVM you will need to build and link another `llvm-opt-fuzzer` with FLUX for each respective version of LLVM.
The steps would be the same as the above example, but instead of building the version of LLVM in the FLUX directory, you will need to clone
and create a build directory in your LLVM version of choice. I.e.:

```bash
> git clone https://github.com/llvm/llvm-project.git
> cd llvm-project
> git checkout release/17.x
> mkdir build-fuzz
> [same steps as above]
```

Note that FLUX has only been tested on versions 16 and 17 of LLVM, so compatibility with other versions is not guaranteed.

## Running FLUX

To fuzz LLVM optimizations, we use can use `llvm-opt-fuzzer` in same manner as specified in the LLVM docs: https://llvm.org/docs/FuzzingLLVM.html#llvm-opt-fuzzer.
The only extra flag needed is `-cross_over=1`, which is required to activate FLUX's crossovers:

```bash
> FLUX/build-fuzz/bin/llvm-opt-fuzzer <corpus-dir> \
                                      -cross_over=1 \
                                      -ignore_remaining_args=1 \
                                      -mtriple x86_64 \
                                      -passes instcombine
```

Note that the `<corpus-dir>` must be comprised of LLVM bitcode files.


### `fuzzer_runner.py` script

During our evaluation, we observed that FLUX frequently found many of the same crashes which was tedious to deduplicate.
Because of this, we also provide a script that runs `llvm-opt-fuzzer` and automatically deduplicates crashes based on specified input directories.
To use this script, we will need to build LLVM a third time to generate a version of LLVM's `opt` that contains debug information:

```bash
> cd FLUX
> mkdir build-debug
> cd build-debug
> cmake -G Ninja \
        -DCMAKE_BUILD_TYPE=Debug \
        -DLLVM_USE_LINKER=lld \
        ../llvm
> ninja opt
```

The script requires a `crashes_dir` argument that contains all previous crash producing LLVM bitcode files. It will use `opt` to generate and hash
the crash output of each bitcode file. Any new crash will compared against existing hashes.

More information on using this script can be found with the following:
```bash
python3 scripts/fuzzer_runner.py --help
```

#### Example usage:

```bash
> mkdir -p fuzzer_output_dirs/crashes \
           fuzzer_output_dirs/output \
           fuzzer_output_dirs/new_crashes \
           fuzzer_output_dirs/seeds
> cp unit-tests/ll_functions/InstCombine___* fuzzer_output_dirs/seeds
> python3 scripts/fuzzer_runner.py \
          FLUX/build-fuzz/bin/llvm-opt-fuzzer \
          FLUX/build-debug/bin/opt \
          fuzzer_output_dirs/crashes \
          fuzzer_output_dirs/output \
          fuzzer_output_dirs/new_crashes \
          fuzzer_output_dirs/seeds \
          x86_64 \
          "default<O3>"
```

## Integration with Alive2

In our paper we find miscompilations with Alive2:

https://github.com/AliveToolkit/alive2

Unfortunately we do not have any direct integration for Alive2 in this repository.
To find miscompilations, we simply iterate over each LLVM IR file generated by FLUX (e.g. the `fuzzer_output_dirs/output` directory in the above example) and feed these tests one by one into Alive2's standalone tool `alive-tv`.
Two points of consideration:
- Alive2 uses Z3 and Z3 has memory limits. We feed FLUX generated tests in from smallest to largest to reduce time wasted on large programs that cannot be verified.
- Alive2 does not support inter-procedural transformations. It is recommended to disable FLUX's sequencing mutation when generating LLVM IR files for Alive2 validation.
