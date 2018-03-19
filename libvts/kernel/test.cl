KERNEL_DEFINES
(
 __kernel void test(void) {
   int idx = get_global_id(0);
   printf("%d thread \n", idx);
 }
 )
