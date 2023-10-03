; ModuleID = 'first-miscomp.bc'
source_filename = "miscomp.ll"

define i32 @bar.SM(i32 %h) {
  %sd = sdiv i32 %h, 2
  %t = icmp sgt i32 %sd, 1
  %sd.i = sdiv i32 %sd, 2
  %t.i = icmp sgt i32 %sd.i, 1
  %r.i = select i1 %t.i, i32 %sd.i, i32 1
  %r = select i1 %t, i32 %sd, i32 %r.i
  ret i32 %r
}
