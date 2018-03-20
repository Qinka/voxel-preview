#pragma once
#ifndef _VTS_VTS_H_
#define _VTS_VTS_H_ 0

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif // __APPLE__

#include <vts_error.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

#ifdef WIN32
#ifdef _VTS_EXPORT_
#define EXPORT __declspec(dllexport)
#else
#define EXPORT __declspec(dllimport)
#endif // _VTS_EXPORT
#else
#define EXPORT

#endif // WIN32

  /**
   * load libraray context
   * the context created will be store in "global_vts_context"
   * @param pid platform id
   * @param did device id (included cpu)
   * @param return vts error
   */
  EXPORT vts_error load_library_context(int pid);

  /**
   * free the context of libraray
   */
  EXPORT void free_library_context();

  // functions

  /**
   * test function
   * @param number of woker
   */
  EXPORT void test_cl_kernel_call(size_t n_worker);

  /**
   * call the kernels
   * @param kernel_name the name of kernel
   * @param calling_kernel is the kernel had been add to queue
   * @param num_args is the number of the kernel. 
   */
  EXPORT
  cl_int call_cl_kernel(const char * kernel_name,
                        cl_kernel* calling_kernel,
                        const int num_args,
                        ...
                        );
  /**
   * get the command queues
   * @return the global command queues
   */
  EXPORT
  const cl_command_queue* get_global_command_queue();

  /**
   * get global context
   * @return the global context
   */
  EXPORT
  const cl_context get_global_context();

  /**
   * get global program
   * @return the global program
   */
  EXPORT
  const cl_program get_global_program();

  /**
   * get devices ids
   * @return list of devices id
   */
  EXPORT
  const cl_device_id* get_global_dids();

  /**
   * get global platform
   * @return the platform
   */
  EXPORT
  const cl_platform_id get_global_platform();

  /**
   * get the number of devices
   * @return number of devices
   */
  EXPORT
  const uint32_t get_num_devices();

  /**
   * Print all platforms with devices
   */
  EXPORT
  void print_all_plat_n_dev();

#ifdef __cplusplus
}
#endif // __cplusplus
#endif // ! _VTS_VTS_H_
