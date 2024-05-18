# Bugs found by FLUX 

A collection of all bugs that FLUX has discovered in LLVM's mid-end optimizations. We categorize bugs as one of:
- **Miscompilation**: Incorrect executable code is silently generated from a correct LLVM IR program
- **LLVM ERROR**: The optimizer transforms a correct LLVM IR program into an invalid state, which crashes the optimizer.

|Filename|Bug Type|Optimization|GitHub Issue|Fixed|
|---|---|---|---|---|
|cvp1.ll|Miscompilation|CorrelatedValuePropagation|[link](https://github.com/llvm/llvm-project/issues/62200)|✅|
|instcombine1.ll|Miscompilation|InstCombine|[link](https://github.com/llvm/llvm-project/issues/62401)|✅|
|agginstcombine1.ll|LLVM ERROR|AggressiveInstCombine|[link](https://github.com/llvm/llvm-project/issues/62509)|✅|
|loopidiom1.ll|Miscompilation|LoopIdiomRecognize|[link](https://github.com/llvm/llvm-project/issues/62873)|✅|
|cvp2.ll|Miscompilation|CorrelatedValuePropagation|[link](https://github.com/llvm/llvm-project/issues/62901)|✅|
|gvn2.ll|Miscompilation|GVN|[link](https://github.com/llvm/llvm-project/issues/63968)|❌|
|instcombine2.ll|Miscompilation|InstCombine|[link](https://github.com/llvm/llvm-project/issues/63992)|✅|
|licm.ll|LLVM ERROR|LICM|[link](https://github.com/llvm/llvm-project/issues/64215)|✅|
|sroa.ll|LLVM ERROR|SROA|[link](https://github.com/llvm/llvm-project/issues/64721)|✅|
