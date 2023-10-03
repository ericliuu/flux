define <4 x float> @simple_select(<4 x float> %a, <4 x float> %b, <4 x i32> %c) {
f_ult.SM.exit:
  %c0 = extractelement <4 x i32> %c, i32 0
  %a0 = extractelement <4 x float> %a, i32 0
  %a1 = extractelement <4 x float> %a, i32 1
  %b0 = extractelement <4 x float> %b, i32 0
  %b1 = extractelement <4 x float> %b, i32 123
  %cmp0 = icmp ne i32 %c0, 0
  %s0 = select i1 %cmp0, float %a0, float %b0
  %s1 = select i1 %cmp0, float %a1, float %b1
  %ra = insertelement <4 x float> <float poison, float poison, float undef, float undef>, float %s0, i32 0
  %rb = insertelement <4 x float> %ra, float %s1, i32 1
  ret <4 x float> %rb
}
