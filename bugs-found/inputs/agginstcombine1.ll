define i64 @load64le_nop_shift.SM0(ptr nocapture %arg) local_unnamed_addr #0 {
  %g1 = getelementptr inbounds i8, ptr %arg, i64 1
  %g6 = getelementptr inbounds i8, ptr %arg, i64 6
  %g7 = getelementptr inbounds i8, ptr %arg, i64 7
  %ld0 = load i8, ptr %arg, align 1
  %ld1 = load i8, ptr %g1, align 1
  %ld6 = load i8, ptr %g6, align 1
  %ld7 = load i8, ptr %g7, align 1
  %z0 = zext i8 %ld0 to i64
  %z1 = zext i8 %ld1 to i64
  %z6 = zext i8 %ld6 to i64
  %z7 = zext i8 %ld7 to i64
  %g2.i = getelementptr inbounds i8, ptr %arg, i64 8
  %ld2.i = load i8, ptr %g2.i, align 1
  %z2.i = zext i8 %ld2.i to i64
  %s1.i = shl nuw nsw i64 %z7, 8
  %s2.i = shl nuw nsw i64 %z2.i, 16
  %1 = or i64 %s1.i, %s2.i
  %o7.i = or i64 %1, %z6
  %s0 = shl nuw nsw i64 %z0, %o7.i
  %s1 = shl nuw nsw i64 %z1, 8
  %o7 = or i64 %s0, %s1
  ret i64 %o7
}
