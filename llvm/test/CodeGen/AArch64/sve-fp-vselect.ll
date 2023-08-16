; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=aarch64-linux-gnu -mattr=+sve < %s | FileCheck %s

define <vscale x 8 x half> @vselect_fmul_f16(<vscale x 8 x i1> %p, <vscale x 8 x half> %a, <vscale x 8 x half> %b) {
; CHECK-LABEL: vselect_fmul_f16:
; CHECK:       // %bb.0:
; CHECK-NEXT:    fmul z0.h, p0/m, z0.h, z1.h
; CHECK-NEXT:    ret
  %mul = fmul <vscale x 8 x half> %a, %b
  %sel = select <vscale x 8 x i1> %p, <vscale x 8 x half> %mul, <vscale x 8 x half> %a
  ret <vscale x 8 x half> %sel
}

define <vscale x 4 x float> @vselect_fmul_f32(<vscale x 4 x i1> %p, <vscale x 4 x float> %a, <vscale x 4 x float> %b) {
; CHECK-LABEL: vselect_fmul_f32:
; CHECK:       // %bb.0:
; CHECK-NEXT:    fmul z0.s, p0/m, z0.s, z1.s
; CHECK-NEXT:    ret
  %mul = fmul <vscale x 4 x float> %a, %b
  %sel = select <vscale x 4 x i1> %p, <vscale x 4 x float> %mul, <vscale x 4 x float> %a
  ret <vscale x 4 x float> %sel
}

define <vscale x 2 x double> @vselect_fmul_f64(<vscale x 2 x i1> %p, <vscale x 2 x double> %a, <vscale x 2 x double> %b) {
; CHECK-LABEL: vselect_fmul_f64:
; CHECK:       // %bb.0:
; CHECK-NEXT:    fmul z0.d, p0/m, z0.d, z1.d
; CHECK-NEXT:    ret
  %mul = fmul <vscale x 2 x double> %a, %b
  %sel = select <vscale x 2 x i1> %p, <vscale x 2 x double> %mul, <vscale x 2 x double> %a
  ret <vscale x 2 x double> %sel
}

define <vscale x 8 x half> @vselect_fadd_f16(<vscale x 8 x i1> %p, <vscale x 8 x half> %a, <vscale x 8 x half> %b) {
; CHECK-LABEL: vselect_fadd_f16:
; CHECK:       // %bb.0:
; CHECK-NEXT:    fadd z0.h, p0/m, z0.h, z1.h
; CHECK-NEXT:    ret
  %add = fadd <vscale x 8 x half> %a, %b
  %sel = select <vscale x 8 x i1> %p, <vscale x 8 x half> %add, <vscale x 8 x half> %a
  ret <vscale x 8 x half> %sel
}

define <vscale x 4 x float> @vselect_fadd_f32(<vscale x 4 x i1> %p, <vscale x 4 x float> %a, <vscale x 4 x float> %b) {
; CHECK-LABEL: vselect_fadd_f32:
; CHECK:       // %bb.0:
; CHECK-NEXT:    fadd z0.s, p0/m, z0.s, z1.s
; CHECK-NEXT:    ret
  %add = fadd <vscale x 4 x float> %a, %b
  %sel = select <vscale x 4 x i1> %p, <vscale x 4 x float> %add, <vscale x 4 x float> %a
  ret <vscale x 4 x float> %sel
}

define <vscale x 2 x double> @vselect_fadd_f64(<vscale x 2 x i1> %p, <vscale x 2 x double> %a, <vscale x 2 x double> %b) {
; CHECK-LABEL: vselect_fadd_f64:
; CHECK:       // %bb.0:
; CHECK-NEXT:    fadd z0.d, p0/m, z0.d, z1.d
; CHECK-NEXT:    ret
  %add = fadd <vscale x 2 x double> %a, %b
  %sel = select <vscale x 2 x i1> %p, <vscale x 2 x double> %add, <vscale x 2 x double> %a
  ret <vscale x 2 x double> %sel
}

define <vscale x 8 x half> @vselect_fsub_f16(<vscale x 8 x i1> %p, <vscale x 8 x half> %a, <vscale x 8 x half> %b) {
; CHECK-LABEL: vselect_fsub_f16:
; CHECK:       // %bb.0:
; CHECK-NEXT:    fsub z0.h, p0/m, z0.h, z1.h
; CHECK-NEXT:    ret
  %sub = fsub <vscale x 8 x half> %a, %b
  %sel = select <vscale x 8 x i1> %p, <vscale x 8 x half> %sub, <vscale x 8 x half> %a
  ret <vscale x 8 x half> %sel
}

define <vscale x 4 x float> @vselect_fsub_f32(<vscale x 4 x i1> %p, <vscale x 4 x float> %a, <vscale x 4 x float> %b) {
; CHECK-LABEL: vselect_fsub_f32:
; CHECK:       // %bb.0:
; CHECK-NEXT:    fsub z0.s, p0/m, z0.s, z1.s
; CHECK-NEXT:    ret
  %sub = fsub <vscale x 4 x float> %a, %b
  %sel = select <vscale x 4 x i1> %p, <vscale x 4 x float> %sub, <vscale x 4 x float> %a
  ret <vscale x 4 x float> %sel
}

define <vscale x 2 x double> @vselect_fsub_f64(<vscale x 2 x i1> %p, <vscale x 2 x double> %a, <vscale x 2 x double> %b) {
; CHECK-LABEL: vselect_fsub_f64:
; CHECK:       // %bb.0:
; CHECK-NEXT:    fsub z0.d, p0/m, z0.d, z1.d
; CHECK-NEXT:    ret
  %sub = fsub <vscale x 2 x double> %a, %b
  %sel = select <vscale x 2 x i1> %p, <vscale x 2 x double> %sub, <vscale x 2 x double> %a
  ret <vscale x 2 x double> %sel
}