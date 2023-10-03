; ModuleID = 'ecebd83de1ae7222a86caf49e23dafdd055e11cc'
source_filename = "func-sequence.zext_bool_urem_divisor_vec.xor_sext_to_sel_constant_vec.SM"

define <2 x i32> @zext_bool_urem_divisor_vec(<2 x i1> %x, <2 x i32> %y) {
  %ext = zext <2 x i1> %x to <2 x i32>
  %sext.i = sext <2 x i1> %x to <2 x i32>
  %sext.i1 = sext <2 x i1> %x to <2 x i32>
  %r.i2 = xor <2 x i32> <i32 42, i32 -7>, %sext.i1
  %r.i = xor <2 x i32> <i32 42, i32 -7>, %r.i2
  %r = urem <2 x i32> %y, %r.i
  ret <2 x i32> %r
}
