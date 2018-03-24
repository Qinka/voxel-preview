KERNEL_DEF
(
 __constant const float edge_offset[] =
   {-0.5,  -0.5,  -0.5,  0.5,  -0.5,  -0.5,
     0.5,  -0.5,  -0.5,  0.5,  0.5,  -0.5,
     0.5,  0.5,  -0.5,  0.5,  0.5,  0.5,
     
     0.5,  -0.5,  -0.5,  0.5,  -0.5,  0.5,
     0.5,  -0.5,  0.5,  0.5,  0.5,  0.5,
     0.5,  0.5,  0.5,  -0.5,  0.5,  0.5,

     0.5,  -0.5,  0.5,  -0.5,  -0.5,  0.5,
     -0.5,  -0.5,  0.5,  -0.5,  0.5,  0.5,
       -0.5,  0.5,  0.5,  -0.5,  0.5,  -0.5,
       
       -0.5,  -0.5,  0.5,  -0.5,  -0.5,  -0.5,
       -0.5,  -0.5,  -0.5,  -0.5,  0.5,  -0.5,
       -0.5,  0.5,  -0.5,  0.5,  0.5,  -0.5,
       };
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

   for(int i = 0; i < 24; ++i) {
     cur[i*3+0] = isless(0,values[idx0]) * (edge_offset[i*3+0] + x - width  / 2);
     cur[i*3+1] = isless(0,values[idx0]) * (edge_offset[i*3+1] + y - height / 2);
     cur[i*3+2] = isless(0,values[idx0]) * (edge_offset[i*3+2] + z - depth  / 2);
   }
 }
 )




KERNEL_DEF
(
 __constant const float face_offset[] =
   {-0.5,  -0.5,  -0.5,  0.5,  -0.5,  -0.5,
       0.5,  0.5,  -0.5,  -0.5,  0.5,  -0.5,

       -0.5,  -0.5,  -0.5,  -0.5,  0.5,  -0.5,
       -0.5,  0.5,  0.5,  -0.5,  -0.5,  0.5,

       -0.5,  -0.5,  -0.5,  0.5,  -0.5,  -0.5,
       0.5,  -0.5,  0.5,  -0.5,  -0.5,  0.5,


       0.5,  0.5,  0.5,  -0.5,  0.5,  0.5,
       -0.5,  -0.5,  0.5,  0.5,  -0.5,  0.5,

       0.5,  0.5,  0.5,  0.5,  -0.5,  0.5,
       0.5,  -0.5,  -0.5,  0.5,  0.5,  -0.5,
     
       0.5,  0.5,  0.5,  -0.5,  0.5,  0.5,
       -0.5,  0.5,  -0.5,  0.5,  0.5,  -0.5,

       };
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

   for(int i = 0; i< 24; ++i) {
     cur[i*3+0] = isless(0,values[idx0]) * (face_offset[i*3+0] + x - width  / 2);
     cur[i*3+1] = isless(0,values[idx0]) * (face_offset[i*3+1] + y - height / 2);
     cur[i*3+2] = isless(0,values[idx0]) * (face_offset[i*3+2] + z - depth  / 2);
   }
 }
 )
