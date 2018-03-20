#pragma once
#ifndef _VTS_KERNELS_H_
#define _VTS_KERNELS_H_ 0

#define KERNEL_DEF(...) "/* " __FILE__ "*/\n" #__VA_ARGS__ "\n\n"

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

#include <vector>
#include <vts_error.h>
#include <string>


/**
 * get source text
 * @return the pointer of the source
 */
const char ** get_kernel_source_text();

/**
 * get the count
 * @return count
 */
unsigned get_kernel_source_count();


#endif // !_VTS_KERNELS_H_
