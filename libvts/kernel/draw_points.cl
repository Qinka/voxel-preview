KERNEL_DEF
(
 struct cuba_points_t {
   float points[72];
 }
 __attribute__((aligned(1)))
 ;
 )

KERNEL_DEF
(
 __kernel
 void edge_points(__global float* points, __global const float* values) {
   int x = get_global_id(0);
   int y = get_global_id(1);
   int z = get_global_id(2);
   int width  = get_global_size(0);
   int height = get_global_size(1);
   int depth  = get_global_size(2);
   int idx0   = x + y * width + z * width * height;
   int idxk   = idx0 * 12 * 2 * 3;
   __global float * cur = points + idxk;

   if (values[idx0] > 0) {
     *cur++ = 0+x;  *cur++ = 0+y;  *cur++ = 0+z;  *cur++ = 1+x;  *cur++ = 0+y;  *cur++ = 0+z;
     *cur++ = 1+x;  *cur++ = 0+y;  *cur++ = 0+z;  *cur++ = 1+x;  *cur++ = 1+y;  *cur++ = 0+z;
     *cur++ = 1+x;  *cur++ = 1+y;  *cur++ = 0+z;  *cur++ = 1+x;  *cur++ = 1+y;  *cur++ = 1+z;

     *cur++ = 1+x;  *cur++ = 0+y;  *cur++ = 0+z;  *cur++ = 1+x;  *cur++ = 0+y;  *cur++ = 1+z;
     *cur++ = 1+x;  *cur++ = 0+y;  *cur++ = 1+z;  *cur++ = 1+x;  *cur++ = 1+y;  *cur++ = 1+z;
     *cur++ = 1+x;  *cur++ = 1+y;  *cur++ = 1+z;  *cur++ = 0+x;  *cur++ = 1+y;  *cur++ = 1+z;

     *cur++ = 1+x;  *cur++ = 0+y;  *cur++ = 1+z;  *cur++ = 0+x;  *cur++ = 0+y;  *cur++ = 1+z;
     *cur++ = 0+x;  *cur++ = 0+y;  *cur++ = 1+z;  *cur++ = 0+x;  *cur++ = 1+y;  *cur++ = 1+z;
     *cur++ = 0+x;  *cur++ = 1+y;  *cur++ = 1+z;  *cur++ = 0+x;  *cur++ = 1+y;  *cur++ = 0+z;

     *cur++ = 0+x;  *cur++ = 0+y;  *cur++ = 1+z;  *cur++ = 0+x;  *cur++ = 0+y;  *cur++ = 0+z;
     *cur++ = 0+x;  *cur++ = 0+y;  *cur++ = 0+z;  *cur++ = 0+x;  *cur++ = 1+y;  *cur++ = 0+z;
     *cur++ = 0+x;  *cur++ = 1+y;  *cur++ = 0+z;  *cur++ = 1+x;  *cur++ = 1+y;  *cur++ = 0+z;
   }
   else {
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;

     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;

     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;

     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
   }
 }
 )

KERNEL_DEF
(
 __kernel
 void face_points(__global float* points, __global const float* values) {
   int x = get_global_id(0);
   int y = get_global_id(1);
   int z = get_global_id(2);
   int width  = get_global_size(0);
   int height = get_global_size(1);
   int depth  = get_global_size(2);
   int idx0   = x + y * width + z * width * height;
   int idxk   = idx0 * 6 * 4 * 3;
   __global float * cur = points + idxk;

   if (values[idx0] > 0) {
     *cur++ = 0+x;  *cur++ = 0+y;  *cur++ = 0+z;  *cur++ = 1+x;  *cur++ = 0+y;  *cur++ = 0+z;
     *cur++ = 1+x;  *cur++ = 1+y;  *cur++ = 0+z;  *cur++ = 0+x;  *cur++ = 1+y;  *cur++ = 0+z;

     *cur++ = 0+x;  *cur++ = 0+y;  *cur++ = 0+z;  *cur++ = 0+x;  *cur++ = 1+y;  *cur++ = 0+z;
     *cur++ = 0+x;  *cur++ = 1+y;  *cur++ = 1+z;  *cur++ = 0+x;  *cur++ = 0+y;  *cur++ = 1+z;

     *cur++ = 0+x;  *cur++ = 0+y;  *cur++ = 0+z;  *cur++ = 1+x;  *cur++ = 0+y;  *cur++ = 0+z;
     *cur++ = 1+x;  *cur++ = 0+y;  *cur++ = 1+z;  *cur++ = 0+x;  *cur++ = 0+y;  *cur++ = 1+z;


     *cur++ = 1+x;  *cur++ = 1+y;  *cur++ = 1+z;  *cur++ = 0+x;  *cur++ = 1+y;  *cur++ = 1+z;
     *cur++ = 0+x;  *cur++ = 0+y;  *cur++ = 1+z;  *cur++ = 1+x;  *cur++ = 0+y;  *cur++ = 1+z;

     *cur++ = 1+x;  *cur++ = 1+y;  *cur++ = 1+z;  *cur++ = 1+x;  *cur++ = 0+y;  *cur++ = 1+z;
     *cur++ = 1+x;  *cur++ = 0+y;  *cur++ = 0+z;  *cur++ = 1+x;  *cur++ = 1+y;  *cur++ = 0+z;

     *cur++ = 1+x;  *cur++ = 1+y;  *cur++ = 1+z;  *cur++ = 0+x;  *cur++ = 1+y;  *cur++ = 1+z;
     *cur++ = 0+x;  *cur++ = 1+y;  *cur++ = 0+z;  *cur++ = 1+x;  *cur++ = 1+y;  *cur++ = 0+z;
   }
   else {
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;

     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;

     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;


     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;

     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;

     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
     *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;  *cur++ = 0;
   }
 }
 )
