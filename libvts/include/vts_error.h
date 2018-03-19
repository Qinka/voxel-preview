#pragma once
#ifndef _VTS_ERROR_H_
#define _VTS_ERROR_H_ 0

#include <stdint.h>

typedef enum {
  VE_SUCCESS = 0,
  VE_UNKNOW  = 1,
  VE_PLATFORM_ERR,
  VE_CL_CONTEXT_ERR,
  VE_OF, // index larger than size
  VE_CREATE_QUEUE_FAIL, // fail to create the queues
  VE_CREATE_PROGRAM_FAIL, // fail to create program
  VE_CREATE_KERNEL_FAIL, // fail to create kernels
} vts_error;

#endif // ! _VTS_ERROR_H_
