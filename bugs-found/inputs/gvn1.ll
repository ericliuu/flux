define i32 @recurrence_2(ptr %a, i32 %n) {
for.preheader:
  %arrayidx2.phi.trans.insert = getelementptr inbounds i32, ptr %a, i64 -1
  %.pre = load i32, ptr %arrayidx2.phi.trans.insert, align 4
  br label %scalar.body

scalar.body:                                      ; preds = %test_04.SM.exit, %for.preheader
  %0 = phi i32 [ %.pre, %for.preheader ], [ %1, %test_04.SM.exit ]
  %indvars.iv = phi i64 [ 0, %for.preheader ], [ %indvars.iv.next, %test_04.SM.exit ]
  %minmax.028 = phi i32 [ poison, %for.preheader ], [ %minmax.0.cond, %test_04.SM.exit ]
  %arrayidx = getelementptr inbounds i32, ptr %a, i64 %indvars.iv
  %1 = load i32, ptr %arrayidx, align 4
  br label %loop.i.peel.begin

loop.i.peel.begin:                                ; preds = %scalar.body
  br label %loop.i.peel

loop.i.peel:                                      ; preds = %loop.i.peel.begin
  %2 = zext i32 0 to i64
  %el.ptr.i.peel = getelementptr i8, ptr %arrayidx2.phi.trans.insert, i64 %2
  %el.i.peel = load i8, ptr %el.ptr.i.peel, align 4
  %bound_check.i.peel = icmp slt i8 %el.i.peel, 0
  br i1 %bound_check.i.peel, label %test_04.SM.exit, label %loop.i.peel.next

loop.i.peel.next:                                 ; preds = %loop.i.peel
  br label %loop.i.peel.next1

loop.i.peel.next1:                                ; preds = %loop.i.peel.next
  br label %scalar.body.peel.newph

scalar.body.peel.newph:                           ; preds = %loop.i.peel.next1
  br label %loop.i

loop.i:                                           ; preds = %loop.i, %scalar.body.peel.newph
  %el.ptr.i = getelementptr i8, ptr %arrayidx2.phi.trans.insert, i64 1
  %el.i = load i8, ptr %el.ptr.i, align 4
  %bound_check.i = icmp slt i8 %el.i, 0
  br i1 %bound_check.i, label %test_04.SM.exit.loopexit, label %loop.i

test_04.SM.exit.loopexit:                         ; preds = %loop.i
  br label %test_04.SM.exit

test_04.SM.exit:                                  ; preds = %test_04.SM.exit.loopexit, %loop.i.peel
  %cmp4 = icmp sgt i32 %0, 0
  %cond = select i1 %cmp4, i32 %0, i32 -1
  %minmax.0.cond = tail call i32 @llvm.smin.i32(i32 %minmax.028, i32 %cond)
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %n
  br i1 %exitcond, label %for.cond.cleanup.loopexit, label %scalar.body

for.cond.cleanup.loopexit:                        ; preds = %test_04.SM.exit
  %minmax.0.cond.lcssa = phi i32 [ %minmax.0.cond, %test_04.SM.exit ]
  ret i32 %minmax.0.cond.lcssa
}

declare i32 @llvm.smin.i32(i32, i32)
