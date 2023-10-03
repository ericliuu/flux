; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=thumbv8.1m.main-none-none-eabi -mattr=+mve %s -o - | FileCheck %s --check-prefix=CHECK-MVE
; RUN: llc -mtriple=thumbv8.1m.main-none-none-eabi -mattr=+mve.fp %s -o - | FileCheck %s --check-prefix=CHECK-MVEFP

define arm_aapcs_vfpcc void @vabd_v4f32(<4 x float> %x, <4 x float> %y, ptr %z) {
; CHECK-MVE-LABEL: vabd_v4f32:
; CHECK-MVE:       @ %bb.0: @ %entry
; CHECK-MVE-NEXT:    .save {r4, r5, r6, r7, r8, r9, lr}
; CHECK-MVE-NEXT:    push.w {r4, r5, r6, r7, r8, r9, lr}
; CHECK-MVE-NEXT:    .pad #4
; CHECK-MVE-NEXT:    sub sp, #4
; CHECK-MVE-NEXT:    .vsave {d8, d9, d10, d11}
; CHECK-MVE-NEXT:    vpush {d8, d9, d10, d11}
; CHECK-MVE-NEXT:    vmov q4, q1
; CHECK-MVE-NEXT:    vmov q5, q0
; CHECK-MVE-NEXT:    mov r8, r0
; CHECK-MVE-NEXT:    vmov r0, r6, d10
; CHECK-MVE-NEXT:    vmov r1, r7, d8
; CHECK-MVE-NEXT:    bl __aeabi_fsub
; CHECK-MVE-NEXT:    mov r9, r0
; CHECK-MVE-NEXT:    mov r0, r6
; CHECK-MVE-NEXT:    mov r1, r7
; CHECK-MVE-NEXT:    bl __aeabi_fsub
; CHECK-MVE-NEXT:    mov r6, r0
; CHECK-MVE-NEXT:    vmov r0, r7, d11
; CHECK-MVE-NEXT:    vmov r1, r4, d9
; CHECK-MVE-NEXT:    bl __aeabi_fsub
; CHECK-MVE-NEXT:    mov r5, r0
; CHECK-MVE-NEXT:    mov r0, r7
; CHECK-MVE-NEXT:    mov r1, r4
; CHECK-MVE-NEXT:    bl __aeabi_fsub
; CHECK-MVE-NEXT:    bic r0, r0, #-2147483648
; CHECK-MVE-NEXT:    vmov s3, r0
; CHECK-MVE-NEXT:    bic r0, r5, #-2147483648
; CHECK-MVE-NEXT:    vmov s2, r0
; CHECK-MVE-NEXT:    bic r0, r6, #-2147483648
; CHECK-MVE-NEXT:    vmov s1, r0
; CHECK-MVE-NEXT:    bic r0, r9, #-2147483648
; CHECK-MVE-NEXT:    vmov s0, r0
; CHECK-MVE-NEXT:    vstrw.32 q0, [r8]
; CHECK-MVE-NEXT:    vpop {d8, d9, d10, d11}
; CHECK-MVE-NEXT:    add sp, #4
; CHECK-MVE-NEXT:    pop.w {r4, r5, r6, r7, r8, r9, pc}
;
; CHECK-MVEFP-LABEL: vabd_v4f32:
; CHECK-MVEFP:       @ %bb.0: @ %entry
; CHECK-MVEFP-NEXT:    vabd.f32 q0, q0, q1
; CHECK-MVEFP-NEXT:    vstrw.32 q0, [r0]
; CHECK-MVEFP-NEXT:    bx lr

entry:
  %0 = fsub <4 x float> %x, %y
  %1 = call <4 x float> @llvm.fabs.v4f32(<4 x float> %0)
  store <4 x float> %1, ptr %z, align 4
  ret void
}

define arm_aapcs_vfpcc void @vabd_v8f16(<8 x half> %x, <8 x half> %y, ptr %z) {
; CHECK-MVE-LABEL: vabd_v8f16:
; CHECK-MVE:       @ %bb.0: @ %entry
; CHECK-MVE-NEXT:    .save {r4, r5, r6, lr}
; CHECK-MVE-NEXT:    push {r4, r5, r6, lr}
; CHECK-MVE-NEXT:    .vsave {d8, d9, d10, d11, d12, d13}
; CHECK-MVE-NEXT:    vpush {d8, d9, d10, d11, d12, d13}
; CHECK-MVE-NEXT:    mov r4, r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q1[1]
; CHECK-MVE-NEXT:    vmov q5, q1
; CHECK-MVE-NEXT:    vmov q4, q0
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r5, r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q4[1]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r1, r5
; CHECK-MVE-NEXT:    bl __aeabi_fsub
; CHECK-MVE-NEXT:    mov r5, r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q5[0]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r6, r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q4[0]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r1, r6
; CHECK-MVE-NEXT:    bl __aeabi_fsub
; CHECK-MVE-NEXT:    bic r0, r0, #-2147483648
; CHECK-MVE-NEXT:    bl __aeabi_f2h
; CHECK-MVE-NEXT:    vmov.16 q6[0], r0
; CHECK-MVE-NEXT:    bic r0, r5, #-2147483648
; CHECK-MVE-NEXT:    bl __aeabi_f2h
; CHECK-MVE-NEXT:    vmov.16 q6[1], r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q5[2]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r5, r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q4[2]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r1, r5
; CHECK-MVE-NEXT:    bl __aeabi_fsub
; CHECK-MVE-NEXT:    bic r0, r0, #-2147483648
; CHECK-MVE-NEXT:    bl __aeabi_f2h
; CHECK-MVE-NEXT:    vmov.16 q6[2], r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q5[3]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r5, r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q4[3]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r1, r5
; CHECK-MVE-NEXT:    bl __aeabi_fsub
; CHECK-MVE-NEXT:    bic r0, r0, #-2147483648
; CHECK-MVE-NEXT:    bl __aeabi_f2h
; CHECK-MVE-NEXT:    vmov.16 q6[3], r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q5[4]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r5, r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q4[4]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r1, r5
; CHECK-MVE-NEXT:    bl __aeabi_fsub
; CHECK-MVE-NEXT:    bic r0, r0, #-2147483648
; CHECK-MVE-NEXT:    bl __aeabi_f2h
; CHECK-MVE-NEXT:    vmov.16 q6[4], r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q5[5]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r5, r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q4[5]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r1, r5
; CHECK-MVE-NEXT:    bl __aeabi_fsub
; CHECK-MVE-NEXT:    bic r0, r0, #-2147483648
; CHECK-MVE-NEXT:    bl __aeabi_f2h
; CHECK-MVE-NEXT:    vmov.16 q6[5], r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q5[6]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r5, r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q4[6]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r1, r5
; CHECK-MVE-NEXT:    bl __aeabi_fsub
; CHECK-MVE-NEXT:    bic r0, r0, #-2147483648
; CHECK-MVE-NEXT:    bl __aeabi_f2h
; CHECK-MVE-NEXT:    vmov.16 q6[6], r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q5[7]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r5, r0
; CHECK-MVE-NEXT:    vmov.u16 r0, q4[7]
; CHECK-MVE-NEXT:    bl __aeabi_h2f
; CHECK-MVE-NEXT:    mov r1, r5
; CHECK-MVE-NEXT:    bl __aeabi_fsub
; CHECK-MVE-NEXT:    bic r0, r0, #-2147483648
; CHECK-MVE-NEXT:    bl __aeabi_f2h
; CHECK-MVE-NEXT:    vmov.16 q6[7], r0
; CHECK-MVE-NEXT:    vstrw.32 q6, [r4]
; CHECK-MVE-NEXT:    vpop {d8, d9, d10, d11, d12, d13}
; CHECK-MVE-NEXT:    pop {r4, r5, r6, pc}
;
; CHECK-MVEFP-LABEL: vabd_v8f16:
; CHECK-MVEFP:       @ %bb.0: @ %entry
; CHECK-MVEFP-NEXT:    vabd.f16 q0, q0, q1
; CHECK-MVEFP-NEXT:    vstrw.32 q0, [r0]
; CHECK-MVEFP-NEXT:    bx lr

entry:
  %0 = fsub <8 x half> %x, %y
  %1 = call <8 x half> @llvm.fabs.v8f16(<8 x half> %0)
  store <8 x half> %1, ptr %z
  ret void
}

declare <4 x float> @llvm.fabs.v4f32(<4 x float>)
declare <8 x half> @llvm.fabs.v8f16(<8 x half>)