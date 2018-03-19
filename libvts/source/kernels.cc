#include <kernels.h>
#include <map>
#include <vts_internal.h>

using namespace std;;


static const char* _kernel_sources = {
  #include <kernel/test.cl>
  " "
};

static const vector<string> kernel_list = {"test"};

static const size_t _source_count = 1;

typedef map<string,cl_kernel> kMap;
static kMap kernel_map = kMap();

const kernel_source get_source_text() {
  return &_kernel_sources;
}

size_t get_source_count() {
  return _source_count;
}

vts_error create_all_kernels() {
  // error codes
  cl_int    errCode = CL_SUCCESS;
  vts_error  rtCode = VE_SUCCESS;

  // create
  for(auto kernel_name: kernel_list) {
    cl_kernel tmp = clCreateKernel(global_cl_program, kernel_name.c_str(),&errCode);
    checkCLRt("Can not create kernels", Error, VE_CREATE_KERNEL_FAIL);
    kernel_map.insert(pair<string,cl_kernel>(kernel_name, tmp));
  }
 Error:
  return rtCode;
}

void free_all_kernels() {
  for(auto item:kernel_map)
    clReleaseKernel(item.second);
  return;
}


cl_kernel get_kernel(std::string kn) {
  auto tmp = kernel_map.find(kn);
  if (tmp == kernel_map.end())
    return 0;
  else
    return tmp->second;
}
