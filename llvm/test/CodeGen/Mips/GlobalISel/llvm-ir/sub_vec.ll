; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -O0 -mtriple=mipsel-linux-gnu -global-isel -mcpu=mips32r5 -mattr=+msa,+fp64,+nan2008 -verify-machineinstrs %s -o -| FileCheck %s -check-prefixes=P5600

define void @sub_v16i8(ptr %a, ptr %b, ptr %c) {
; P5600-LABEL: sub_v16i8:
; P5600:       # %bb.0: # %entry
; P5600-NEXT:    ld.b $w1, 0($4)
; P5600-NEXT:    ld.b $w0, 0($5)
; P5600-NEXT:    subv.b $w0, $w0, $w1
; P5600-NEXT:    st.b $w0, 0($6)
; P5600-NEXT:    jr $ra
; P5600-NEXT:    nop
entry:
  %0 = load <16 x i8>, ptr %a, align 16
  %1 = load <16 x i8>, ptr %b, align 16
  %sub = sub <16 x i8> %1, %0
  store <16 x i8> %sub, ptr %c, align 16
  ret void
}

define void @sub_v8i16(ptr %a, ptr %b, ptr %c) {
; P5600-LABEL: sub_v8i16:
; P5600:       # %bb.0: # %entry
; P5600-NEXT:    ld.h $w1, 0($4)
; P5600-NEXT:    ld.h $w0, 0($5)
; P5600-NEXT:    subv.h $w0, $w0, $w1
; P5600-NEXT:    st.h $w0, 0($6)
; P5600-NEXT:    jr $ra
; P5600-NEXT:    nop
entry:
  %0 = load <8 x i16>, ptr %a, align 16
  %1 = load <8 x i16>, ptr %b, align 16
  %sub = sub <8 x i16> %1, %0
  store <8 x i16> %sub, ptr %c, align 16
  ret void
}

define void @sub_v4i32(ptr %a, ptr %b, ptr %c) {
; P5600-LABEL: sub_v4i32:
; P5600:       # %bb.0: # %entry
; P5600-NEXT:    ld.w $w1, 0($4)
; P5600-NEXT:    ld.w $w0, 0($5)
; P5600-NEXT:    subv.w $w0, $w0, $w1
; P5600-NEXT:    st.w $w0, 0($6)
; P5600-NEXT:    jr $ra
; P5600-NEXT:    nop
entry:
  %0 = load <4 x i32>, ptr %a, align 16
  %1 = load <4 x i32>, ptr %b, align 16
  %sub = sub <4 x i32> %1, %0
  store <4 x i32> %sub, ptr %c, align 16
  ret void
}

define void @sub_v2i64(ptr %a, ptr %b, ptr %c) {
; P5600-LABEL: sub_v2i64:
; P5600:       # %bb.0: # %entry
; P5600-NEXT:    ld.d $w1, 0($4)
; P5600-NEXT:    ld.d $w0, 0($5)
; P5600-NEXT:    subv.d $w0, $w0, $w1
; P5600-NEXT:    st.d $w0, 0($6)
; P5600-NEXT:    jr $ra
; P5600-NEXT:    nop
entry:
  %0 = load <2 x i64>, ptr %a, align 16
  %1 = load <2 x i64>, ptr %b, align 16
  %sub = sub <2 x i64> %1, %0
  store <2 x i64> %sub, ptr %c, align 16
  ret void
}