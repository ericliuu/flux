# FLUX-libFuzzer

FLUX (**F**inding bugs with **L**LVM IR based **U**nit test cross(**X**)overs) is a libFuzzer extension designed to find bugs in LLVM's optimization passes.
FLUX accomplishes this by randomly combining pairs of LLVM IR files from LLVM's existing unit test suite (i.e. `llvm/test/Transforms`).

FLUX is built on version LLVM 16.0 (Specifically commit `0303eafcb34647f7d5a4015aad266b5766f5dc5e`)

## Summary

FLUX's fuzzing loop repeatedly combines IR files that are supplied in the seed corpus.
In each iteration the resulting program is fed into LLVM's optimizer and checked for validity.
We build FLUX on top of LLVM's existing `llvm-opt-fuzzer` which itself is built on `libFuzzer`.

## Collecting Unit Tests for Fuzzing

We provide a script that collects all LLVM transformation unit tests into a single directory which can be passed into libFuzzer as a corpus.
To use the script enter the following steps:
```bash
> cd FLUX
> mkdir build
> cd build
> cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DLLVM_USE_LINKER=lld ../llvm
> ninja
> cd ../../
> python3 scripts/collect_unittest_functions.py 

```
By default, the script reads from the unit tests located in the `FLUX` subdirectory, but this can be easily changed by specifying a different path in the above script.
The script will generate four subdirectories in the `unit-tests` directory. The `ll-{files,functions}` directories hold the original unit tests and the unit tests with each individual function extracted into its own file, respectively. The `bc-{files,functions}` directories hold the compiled results of their corresponding `.ll` files.
