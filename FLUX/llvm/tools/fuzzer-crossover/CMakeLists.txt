set(LLVM_LINK_COMPONENTS
  FuzzMutate
  Support
  TransformUtils
  Passes
)

add_llvm_tool(fuzzer-crossover
  fuzzer-crossover.cpp

  DEPENDS
  intrinsics_gen
  )
