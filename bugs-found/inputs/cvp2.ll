define i64 @test43(i32 %a) {
  %is_a_nonnegative = icmp sgt i32 %a, 1
  %narrow = select i1 %is_a_nonnegative, i32 %a, i32 0
  %max = sext i32 %narrow to i64
  ret i64 %max
}
