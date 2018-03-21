#pragma once

#ifndef _VTS_VIEW_H_
#define _VTS_VIEW_H_ 0


#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif // __APPLE__

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include <vts.h>
#include <vts-error.h>

/**
 * Struct of computing, which is what hold system need.
 */
struct computing_reg_f {
  size_t depth; // depth of voxel tensor
  size_t width; // width
  size_t height; // height
  float scal; // scal of voxel (for color)
  float bottom; // bottom value
  float top; // top value
  float* voxel_tensor; // pointer of voxel tensor; depth x width x height
  float* limit_tensor; // transformed with scal and limitf same shape with voxel_tensor
  float* edge_points; // pointer of list of points to draw edges; 72 x depth x width x height
  float* face_points; // pointer of list of points to draw faces; 72 x depth x width x height
  float* face_colors; // pointer of list of colors to draw faces;  3 x depth x width x height
};

/**
 * the consturctor of computing_reg_f
 * this consturctor will it with "host" pointers
 * @param depth the depth of tensor
 * @param width the width of tensor
 * @parma height the height of tensor
 * @return pointer of crf, if failed, it will return NULL.
 * the defaults of scal, bottom, and top is 1,0,1; // TODO change limitf kernel to <= and >=
 */
computing_reg_f* create_crf(size_t depth, size_t width, size_t height);

/**
 * release a computing_reg_f
 * @param crf pointer of computing_reg_f
 */
void release_crf(computing_reg_f * crf);


/**
 * device computing context (struct)
 */
struct device_contexts {
  cl_mem d_voxel_tensor;
  cl_mem d_limit_tensor;
  cl_mem d_edge_points;
  cl_mem d_face_points;
  cl_mem d_face_color;
  // there should be kernels
};

/**
 * create device_contexts
 */

device_contexts* create_dev_context();

/**
 * release device_contexts
 */
void release_dev_context(device_contexts* dc);


/**
 * add the limiting computing to queue
 */
cl_int add_limit_computing();

/**
 * add the edges points computing (depended on limiting computing)
 */
cl_int add_edgeps_computing();

/**
 * add the faces points computing (depended on limiting computing)
 */
cl_int add_faceps_computing();

/**
 * add the color computing (depended on limiting computing)
 */
cl_int add_color_computing();

/**
 * sycn computing; wait for finish
 */
cl_int sycn_computing();

/**
 * copy memorys
 * this function will(should) copy the new one
 */
cl_int copy_memory();



#endif // !_VTS_VIEW_H_ 
