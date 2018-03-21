KERNEL_DEF
(
 __kernel
 void scalef(__global float* to, __global const float* from, const float scal) {
   int x = get_global_id(0);
   to[x] = scal * from[x];
 }

 __kernel
 void limitf(__global float* to, __global const float* from,  const float top, const float bottom) {
   int x = get_global_id(0);
   float v = from[x];
   /*to[x] = v * sign(fmax(bottom,v) - bottom) * sign(top - fmin(top,v));*/
   to[x] = v * isgreater(v,bottom) * isless(v,top); /* v * (v >= a) * (v <= b) */
 }
 )
