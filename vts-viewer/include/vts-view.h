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
#include <vts_error.h>

/**
 * context of computing, which is what hold system need.
 */
struct computing_context {
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
 * the consturctor of computing_context
 * this consturctor will it with "host" pointers
 * @param depth the depth of tensor
 * @param width the width of tensor
 * @parma height the height of tensor
 * @return pointer of context, if failed, it will return NULL.
 * the defaults of scal, bottom, and top is 1,0,1; // TODO change limitf kernel to <= and >=
 */
struct computing_context* create_context(size_t depth, size_t width, size_t height);

/**
 * release a computing_context
 * @param context pointer of computing_context
 */
void release_context(struct computing_context* context);


/**
 * device computing context (struct)
 */
struct device_contexts {
  int   dev_idx;
  cl_mem d_voxel_tensor;
  cl_mem d_limit_tensor;
  cl_mem d_edge_points;
  cl_mem d_face_points;
  cl_mem d_face_colors;
  cl_kernel k_scalef;
  cl_kernel k_limitf;
  cl_kernel k_edge_points;
  cl_kernel k_face_points;
  cl_kernel k_face_colors;
};

/**
 * create device_contexts
 * @parma cc computing contexts
 * @return the device contexts
 */
struct device_contexts* create_dev_context(struct computing_context* cc, int dev_idx);

/**
 * release device_contexts
 * @param dc device contexts
 */
void release_dev_context(struct device_contexts* dc);

/**
 * add the scale limiting computing to queue
 * @parma cc computing contexts
 * @param dc device contexts
 */
cl_int add_scale_computing(struct computing_context* cc, struct device_contexts* dc);
cl_int add_limit_computing(struct computing_context* cc, struct device_contexts* dc);

/**
 * add the edges points computing (depended on limiting computing)
 */
cl_int add_edgeps_computing(struct computing_context* cc, struct device_contexts* dc);

/**
 * add the faces points computing (depended on limiting computing)
 */
cl_int add_faceps_computing(struct computing_context* cc, struct device_contexts* dc);

/**
 * add the color computing (depended on limiting computing)
 */
cl_int add_color_computing(struct computing_context* cc, struct device_contexts* dc);

/**
 * sync computing; wait for finish
 */
cl_int sync_computing(struct computing_context* cc, struct device_contexts* dc);

/**
 * copy memorys
 * this function will(should) copy the new one
 */
cl_int copy_memory_voxel_tensor(struct computing_context* cc, struct device_contexts* dc);
cl_int copy_memory_edge_points(struct computing_context* cc, struct device_contexts* dc);
cl_int copy_memory_face_points(struct computing_context* cc, struct device_contexts* dc);
cl_int copy_memory_face_colors(struct computing_context* cc, struct device_contexts* dc);

#endif // !_VTS_VIEW_H_ 
