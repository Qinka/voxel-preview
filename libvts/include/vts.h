#pragma once
#ifndef _VTS_VTS_H_
#define _VTS_VTS_H_ 0

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif // __APPLE__

#include <vts_error.h>

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
  EXPORT vts_error load_library_context(int pid, int did);

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


#ifdef __cplusplus
}
#endif // __cplusplus
#endif // ! _VTS_VTS_H_
