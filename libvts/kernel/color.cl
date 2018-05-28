KERNEL_DEF
(
 __kernel
 void color_filling(__global float* colors, __global const float* values) {
   int x = get_global_id(0);
   int y = get_global_id(1);
   int z = get_global_id(2);
   int width  = get_global_size(0);
   int height = get_global_size(1);
   int depth  = get_global_size(2);
   int idx0   = x + y * width + z * width * height;
   int idxk   = idx0 * 72;

   __global float * cur = colors + idxk;
   for(int i = 0 ; i < 24; ++i) {
     *cur++ = 0;
     *cur++ = 0;
     *cur++ = values[idx0];
   }
 }
)
