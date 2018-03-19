#include <vts.h>

int main() {
  load_library_context(0,0);
  test_cl_kernel_call(20);
  free_library_context();
}
