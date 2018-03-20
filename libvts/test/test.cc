#include <vts.h>

int main() {
  print_all_plat_n_dev();
  load_library_context(0);
  test_cl_kernel_call(20);
  free_library_context();
}
