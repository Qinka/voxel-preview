#include <vts_internal.h>
#include <kernels.h>
#include <vts.h>
#include <vector>
#include <stdarg.h>

using namespace std;


vts_error load_library_context(int pid) {
  // error codes
  cl_int    errCode = CL_SUCCESS;
  vts_error vtsCode = VE_SUCCESS;
  vts_error  rtCode = VE_SUCCESS;

  // variables
  auto &context  = global_context;
  auto platforms = vector<cl_platform_id>();
  auto &platform = global_platform;
  auto &devices  = global_dids;
  auto &cqueues  = global_command_queues;
  auto &program  = global_program;

  /***** inits *****/

  // load platforms
  load_platforms(platforms);

  // load devices and context
  if(pid < platforms.size()) {
    platform = platforms[pid];
    load_devices(devices, context, platform);
  }
  else {
    rtCode = VE_OF;
    goto Error;
  }
  cqueues = vector<cl_command_queue>(devices.size());

  // create command queue
  for (auto idx = 0; idx < devices.size(); idx++) {
    cqueues[idx] =
      clCreateCommandQueue(global_context,devices[idx],0,&errCode);
    checkCLRt("create queue failed",ErrorQU,VE_CREATE_QUEUE_FAIL);
  }

  // create program
  program =
    clCreateProgramWithSource
    ( context,
      get_kernel_source_count(),
      get_kernel_source_text(),
      0,
      &errCode);
  checkCLRt("create program failed", ErrorPG, VE_CREATE_PROGRAM_FAIL);
  // build program
  checkCL(clBuildProgram(program,devices.size(),&devices[0],"",0,0),
          "Can not build program",
          ErrorPG, VE_CREATE_PROGRAM_FAIL);

  goto Error;

 ErrorPG:
  {
    cout << *get_kernel_source_text() << endl;
    size_t length;
    char* buffer;
    clGetProgramBuildInfo(program, devices[0], CL_PROGRAM_BUILD_LOG, 0, nullptr, &length);
    buffer = new char[length];
    clGetProgramBuildInfo(program, devices[0], CL_PROGRAM_BUILD_LOG, length, buffer, nullptr);
    cout << buffer << endl;
    delete[] buffer;
  }
  clReleaseProgram(program);
 ErrorQU:
  for(auto queue:cqueues)
    {clReleaseCommandQueue(queue);}
 ErrorCT:
  clReleaseContext(context);
 Error:
  return rtCode;
}

void free_library_context(){
  clReleaseProgram(global_program);
  for(auto queue:global_command_queues)
    {clReleaseCommandQueue(queue);}
  clReleaseContext(global_context);
  return;
}

cl_kernel callable_kernel(const char * kernel_name,
                          cl_int*      errCode,
                          const int    num_args,
                          ...
                          ) {
  cl_kernel rt = 0;
  *errCode = CL_SUCCESS;

  va_list ap;
  va_start(ap,num_args);

  rt = clCreateKernel(global_program, kernel_name, errCode);

  /*rt = clCloneKernel(get_kernel(string(kernel_name)), &errCode);
    checkIn(errCode, != , CL_SUCCESS, "can not create kernel",Error);*/

  for(auto i = 0;  i < num_args; ++i){
    size_t size = va_arg(ap,size_t);
    void* ptr = va_arg(ap,void*);
    *errCode = clSetKernelArg(rt, i, size, ptr);
    checkIn(errCode, !=, CL_SUCCESS, "can not add arg of kernel",EndVa);
  }

  goto EndVa;

 EndVa:
  va_end(ap);
 Error:
  return rt;
}


const cl_command_queue* get_global_command_queue() {
  return &global_command_queues[0];
}
const cl_context get_global_context(){
  return global_context;
}
const cl_program get_global_program(){
  return global_program;
}
const cl_device_id* get_global_dids(){
  return &global_dids[0];
}
const cl_platform_id get_global_platform() {
  return global_platform;
}
const uint32_t get_num_devices(){
  return global_dids.size();
}


void test_cl_kernel_call(size_t n_worker) {
  auto errCode = CL_SUCCESS;
  auto test_kernel = callable_kernel("test",&errCode,0);
  cout << errCode << endl;
  clEnqueueNDRangeKernel
    ( global_command_queues[0],
      test_kernel, 1, 0, &n_worker,0,0,0,0);
  clFinish(global_command_queues[0]);

}


void print_all_plat_n_dev(){
  cl_int err;
  cl_uint num;
  err = clGetPlatformIDs(0, 0, &num);
  if(err != CL_SUCCESS) {
    std::cerr << "Unable to get platforms" << endl;
    return;
  }

  vector<cl_platform_id> platforms(num);
  err = clGetPlatformIDs(num, &platforms[0], &num);
  if(err != CL_SUCCESS) {
    cerr << "Unable to get platform ID" << endl;
    return;
  }

  for (auto platform : platforms) {
    cl_context_properties prop[] = { CL_CONTEXT_PLATFORM, reinterpret_cast<cl_context_properties>(platform), 0 };

    cl_context context = clCreateContextFromType(prop, CL_DEVICE_TYPE_ALL, NULL, NULL, NULL);
    if(context == 0) {
      cerr << "Can't create OpenCL context" << endl;
      return;
    }
    size_t cb;
    clGetContextInfo(context, CL_CONTEXT_DEVICES, 0, NULL, &cb);
    vector<cl_device_id> devices(cb / sizeof(cl_device_id));
    clGetContextInfo(context, CL_CONTEXT_DEVICES, cb, &devices[0], 0);

    char *devname;
    for(auto device : devices) {
      clGetDeviceInfo(device, CL_DEVICE_NAME, 0, NULL, &cb);
      devname = new char[cb+2];
      clGetDeviceInfo(device, CL_DEVICE_NAME, cb, &devname[0], 0);
      cerr << "Device: " << devname << endl;
      delete[] devname;
    }
    clReleaseContext(context);
  }
}
