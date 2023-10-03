; ModuleID = '96bb97f95533b463d54c3bff23965419eb14f6dc'
source_filename = "func-sequence.zext_or_masked_bit_test.p2_different_liveout.SM"

define i32 @zext_or_masked_bit_test(i1 %a, i32 %b) {
  %z = zext i1 %a to i32
  %bitmask.i = shl i32 1, %z
  br label %loop.i

loop.i:                                           ; preds = %loop.i, %0
  %curr.i = phi i32 [ %b, %0 ], [ %next.i, %loop.i ]
  %curr.bitmasked.i = and i32 %curr.i, %bitmask.i
  %curr.isbitunset.i = icmp eq i32 %curr.bitmasked.i, 0
  %next.i = shl i32 %curr.i, 1
  br i1 %curr.isbitunset.i, label %loop.i, label %p2_different_liveout.SM.exit

p2_different_liveout.SM.exit:                     ; preds = %loop.i
  ret i32 %next.i
}
