KERNEL_DEF
(
 __kernel void cast_u8_f(__global float* to, __global const unsigned char* from) {
   int x = get_global_id(0);
   to[x] = (float) from[x];
 }
 __kernel void cast_f_u8(__global unsigned char* to, __global const float* from) {
   int x = get_global_id(0);
   to[x] = (unsigned char) from[x];
 }
)
