define i32 @foo() {
  %A = alloca i1, align 1
  %1 = load atomic volatile i32, ptr %A seq_cst, align 4
  ret i32 %1
}
