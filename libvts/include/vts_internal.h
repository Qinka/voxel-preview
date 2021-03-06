#pragma once

#ifndef _VTS_INTERNAL_H_
#define _VTS_INTERNAL_H_ 0

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif // __APPLE__

#include <vector>
#include <vts_error.h>
#include <iostream>
#include <stdint.h>

/// global vts context
extern cl_context                    global_context;
/// global command queue
extern std::vector<cl_command_queue> global_command_queues;
/// global CL program
extern cl_program                    global_program;
/// global dids
extern std::vector<cl_device_id>     global_dids;
/// global platform
extern cl_platform_id                global_platform;


/**
 * load all platform infos
 * @param platforms (output) vector of platforms
 * @return vts error
 */
vts_error load_platforms(std::vector<cl_platform_id> &platforms);

/**
 * load all device infos
 * @param devices vector of devices
 * @param platform the platform
 * @param vts error
 */
vts_error load_devices(std::vector<cl_device_id> &devices, cl_context &context, cl_platform_id platform);


#define checkRt(err, op, var, errInfo, errLabel, rtValue) \
  if(err op var) {                                        \
    std::cerr << err     << std::endl                     \
              << errInfo << std::endl;                    \
    rtCode = rtValue;                                     \
    goto errLabel;                                        \
  }

#define checkIn(err, op, var, errInfo, errLabel) \
  if(err op var) {                               \
    std::cerr << err     << std::endl                \
              << errInfo << std::endl;           \
    goto errLabel;                               \
  }

#define checkE(cmd, rtVar, op, succValue, errInfo, errLabel, rtValue) \
  rtVar = cmd;                                                        \
  checkRt(rtVar, op, succValue, errInfo, errLabel, rtValue);

#define checkCL(cmd, errInfo, errLabel, rtValue)            \
  checkE(cmd, errCode, !=, CL_SUCCESS, errInfo, errLabel, rtValue);

#define checkCLRt(errInfo, errLabel, rtValue)    \
  checkRt(errCode, !=, CL_SUCCESS, errInfo, errLabel, rtValue)

#endif // ! _VTS_INTERNAL_H_
