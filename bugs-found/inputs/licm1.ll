; ModuleID = 'crash-07cf53972d427275ee35f1ff6b2a868b86d332de'
source_filename = "/home/eric/Repos/llvm-unittest-composition/llvm/test/Transforms/InstCombine/freeze.ll"

declare void @use_i32(i32)

define i32 @freeze_callbr_use_after_phi(i1 %c) {
entry:
  %x = callbr i32 asm sideeffect "", "=r"()
          to label %callbr.cont []

callbr.cont:                                      ; preds = %callbr.cont, %entry
  %phi = phi i32 [ %x, %entry ], [ 0, %callbr.cont ]
  call void @use_i32(i32 %x)
  %fr = freeze i32 %x
  call void @use_i32(i32 %fr)
  call void @use_i32(i32 %phi)
  br label %callbr.cont
}
