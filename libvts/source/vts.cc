#include <vts_internal.h>
#include <kernels.h>
#include <vts.h>
#include <vector>

using namespace std;


vts_error load_library_context(int pid, int did) {
  // error codes
  cl_int    errCode = CL_SUCCESS;
  vts_error vtsCode = VE_SUCCESS;
  vts_error  rtCode = VE_SUCCESS;
  // variables
  auto platforms = vector<cl_platform_id>();
  auto &devices   = global_vts_dids;
  // inits
  // device id
  global_cl_did = did;
  // load platforms
  load_platforms(platforms);
  // load devices and context
  if(pid < platforms.size())
    load_devices(devices, global_vts_context, platforms[pid]);
  else {
    rtCode = VE_OF;
    goto Error;
  }
  global_vts_command_queues = vector<cl_command_queue>(devices.size());
  // create command queue
  for (auto idx = 0; idx < devices.size(); idx++) {
    global_vts_command_queues[idx] =
      clCreateCommandQueue(global_vts_context,devices[idx],0,&errCode);
    checkCLRt("create queue failed",ErrorQU,VE_CREATE_QUEUE_FAIL);
  }
  // create program
  global_cl_program =
    clCreateProgramWithSource
    ( global_vts_context,
      get_source_count(),
      get_source_text(),
      0,
      &errCode);
  checkCLRt("create program failed", ErrorPG, VE_CREATE_PROGRAM_FAIL);
  // build program 
  checkCL(clBuildProgram(global_cl_program,global_vts_dids.size(),&global_vts_dids[0],0,0,0),
          "Can not build program",
          ErrorPG, VE_CREATE_PROGRAM_FAIL);
  
  checkE(create_all_kernels(),
         vtsCode,!=,VE_SUCCESS,
         "Can not create kernels", ErrorK, vtsCode);
  goto Error;

 ErrorK:
  free_all_kernels();
 ErrorPG:
  clReleaseProgram(global_cl_program);
 ErrorQU:
  for(auto queue:global_vts_command_queues)
    {clReleaseCommandQueue(queue);}
 ErrorCT:
  clReleaseContext(global_vts_context);
 Error:
  return rtCode;
}

void free_library_context(){
  free_all_kernels();
  clReleaseProgram(global_cl_program);
  for(auto queue:global_vts_command_queues)
    {clReleaseCommandQueue(queue);}
  clReleaseContext(global_vts_context);
  return;
}

void test_cl_kernel_call(size_t n_worker) {
  clEnqueueNDRangeKernel
    ( global_vts_command_queues[global_cl_did],
      get_kernel("test"), 1, 0, &n_worker,0,0,0,0);
  clFinish(global_vts_command_queues[global_cl_did]);

}
