#pragma once
#ifndef _VTS_KERNELS_H_
#define _VTS_KERNELS_H_ 0

#define KERNEL_DEFINES(str) #str

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
const char ** get_source_text();

/**
 * get the count
 * @return count
 */
unsigned get_source_count();

/**
 * create all the kernels
 * this function should only be called in the function
 * where create context and programs
 * WARNING this function does not checkout the context and programs
 */
vts_error create_all_kernels();

/**
 * release kernels
 * this function should not be called 
 * WARNING this function does not checkout the context and programs
 */
void free_all_kernels();

/**
 * get kernels
 * @param kn kernel name
 * @return kernel
 */
cl_kernel get_kernel(std::string kn);

#endif // !_VTS_KERNELS_H_
