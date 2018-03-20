#include <vts_internal.h>

using namespace std;

/// global vts context
cl_context               global_context = nullptr;
/// global command queue
vector<cl_command_queue> global_command_queues = vector<cl_command_queue>();
/// global CL program
cl_program               global_program = nullptr;
/// global dids
vector<cl_device_id>     global_dids = vector<cl_device_id>();
/// global platform
cl_platform_id           global_platform = nullptr;

vts_error load_platforms(vector<cl_platform_id> &platforms) {
  // error status
  cl_int    errCode = CL_SUCCESS;
  vts_error  rtCode = VE_SUCCESS;

  // number of platform
  cl_uint num;

  // call the function to get the number of platforms
  checkCL(clGetPlatformIDs(0, 0, &num),
        "Unable to get platforms",
        Error,
        VE_PLATFORM_ERR);

  // std::vector<cl_platform_id> platforms(num);
  // resize vector
  platforms.resize(num);

  // call the function to fetch all the platforms infos
  checkCL(clGetPlatformIDs(num, &platforms[0], &num),
          "Unable to get the platforms",
          Error,
          VE_PLATFORM_ERR);
  Error:
    return rtCode;
}

vts_error load_devices(vector<cl_device_id> &devices, cl_context &context, cl_platform_id platform) {
  // error status
  cl_int    errCode = CL_SUCCESS;
  vts_error  rtCode = VE_SUCCESS;

  // context of properties
  cl_context_properties prop[] =
    {CL_CONTEXT_PLATFORM, reinterpret_cast<cl_context_properties>(platform), 0};

  size_t cb;

  checkE(clCreateContextFromType(prop, CL_DEVICE_TYPE_ALL, NULL, NULL, NULL),
         context, ==, 0, "Cannot create context", Error, VE_CL_CONTEXT_ERR);

  clGetContextInfo(context, CL_CONTEXT_DEVICES, 0, NULL, &cb);
  devices.resize(cb / sizeof(cl_device_id));

  checkCL(clGetContextInfo(context, CL_CONTEXT_DEVICES, cb, &devices[0], 0),
          "Cannot get context", Error, VE_CL_CONTEXT_ERR);
 Error:
  return rtCode;
}
