#include <vts-view.h>
#include <assert.h>

///////////////////

#define checkCL(ec, op, val, text, errLabel)            \
  if(ec op val) {                                       \
    printf("%s %s \nerror code: %d\n infos: %s \n",     \
           __FILE__, __LINE__, ec,text);                \
    goto errLabel;                                      \
  }


///////////////////

struct computing_context* create_context(size_t depth, size_t width, size_t height) {

  const size_t num = depth * width * height;
  struct computing_context* ptr = NULL;
  float*              vt = NULL;
  float*              lt = NULL;
  float*              fp = NULL;
  float*              ep = NULL;
  float*              fc = NULL;

  ptr =  malloc(sizeof(struct computing_context));
  if (!ptr) goto Fail;


  ptr -> depth        = depth;
  ptr -> width        = width;
  ptr -> height       = height;
  ptr -> scal         = 1;
  ptr -> bottom       = 0;
  ptr -> top          = 1.1;

  vt = malloc(sizeof(float) * num);
  if(!vt) goto FreeCONTEXT;

  lt = malloc(sizeof(float) * num);
  if(!lt) goto FreeVT;

  ep = malloc(sizeof(float) * num * 72);
  if(!ep) goto FreeLT;

  fp = malloc(sizeof(float) * num * 72);
  if(!fp) goto FreeEP;

  fc = malloc(sizeof(float) * num * 3);
  if(!fc) goto FreeFP;

  ptr -> voxel_tensor = vt;
  ptr -> limit_tensor = lt;
  ptr -> edge_points  = ep;
  ptr -> face_points  = fp;
  ptr -> face_colors  = fc;

  goto Succ;

 FreeFP:
  free(fp);
 FreeEP:
  free(ep);
 FreeLT:
  free(lt);
 FreeVT:
  free(vt);
 FreeCONTEXT:
  free(ptr);
 Fail:
  ptr = NULL;
 Succ:
  return ptr;
}

void release_context(struct computing_context * context) {
  free(context -> voxel_tensor);
  free(context -> limit_tensor);
  free(context -> edge_points);
  free(context -> face_points);
  free(context -> face_colors);
  free(context);
}


struct device_contexts* create_dev_context(struct computing_context* cc, int dev_idx) {
  assert(cc != NULL);


  cl_int errCode = CL_SUCCESS;
  size_t base = cc->depth * cc->width * cc->height;

  struct device_contexts* dc = malloc(sizeof(struct device_contexts));
  if(!dc) goto Fail;

  dc->dev_idx = dev_idx;
  dc->d_voxel_tensor = clCreateBuffer(get_global_context(),
                                      CL_MEM_READ_WRITE,
                                      sizeof(float) * base,
                                      NULL, &errCode);
  checkCL(errCode, !=, CL_SUCCESS, "faile to alloc device memory " ,FreeDC);
 
  dc->d_limit_tensor = clCreateBuffer(get_global_context(),
                                      CL_MEM_READ_WRITE,
                                      sizeof(float) * base,
                                      NULL, &errCode);
  checkCL(errCode, !=, CL_SUCCESS, "faile to alloc device memory " ,FreeDVT);

  dc->d_edge_points = clCreateBuffer(get_global_context(),
                                     CL_MEM_READ_WRITE,
                                     sizeof(float) * base * 72,
                                     NULL, &errCode);
  checkCL(errCode, !=, CL_SUCCESS, "faile to alloc device memory " ,FreeDLT);

  dc->d_face_points = clCreateBuffer(get_global_context(),
                                     CL_MEM_READ_WRITE,
                                     sizeof(float) * base * 72,
                                     NULL, &errCode);
  checkCL(errCode, !=, CL_SUCCESS, "faile to alloc device memory " ,FreeDEP);

  dc->d_face_colors = clCreateBuffer(get_global_context(),
                                    CL_MEM_READ_WRITE,
                                    sizeof(float) * base * 3,
                                    NULL, &errCode);
  checkCL(errCode, !=, CL_SUCCESS, "faile to alloc device memory " ,FreeDFP);

  dc->k_scalef = callable_kernel("scalef",&errCode,3,
                                 sizeof(cl_mem),&(dc->d_limit_tensor),
                                 sizeof(cl_mem),&(dc->d_voxel_tensor),
                                 sizeof(float), &(cc->scal));
  checkCL(errCode, !=, CL_SUCCESS, "faile to create kernel " ,FreeDFC);
  
  dc->k_limitf = callable_kernel("limitf",&errCode,4,
                                 sizeof(cl_mem),&(dc->d_limit_tensor),
                                 sizeof(cl_mem),&(dc->d_limit_tensor),
                                 sizeof(float), &(cc->bottom),
                                 sizeof(float), &(cc->top));
  checkCL(errCode, !=, CL_SUCCESS, "faile to create kernel " ,FreeKS);
  
  dc->k_edge_points = callable_kernel("edge_points",&errCode,2,
                                      sizeof(cl_mem),&(dc->d_edge_points),
                                      sizeof(cl_mem),&(dc->d_limit_tensor));
  checkCL(errCode, !=, CL_SUCCESS, "faile to create kernel " ,FreeKL);
  
  dc->k_face_points = callable_kernel("face_points",&errCode,2,
                                      sizeof(cl_mem),&(dc->d_face_points),
                                      sizeof(cl_mem),&(dc->d_limit_tensor));
  checkCL(errCode, !=, CL_SUCCESS, "faile to create kernel " ,FreeKEP);

  dc->k_face_colors = callable_kernel("color_filling",&errCode,2,
                                      sizeof(cl_mem),&(dc->d_face_colors),
                                      sizeof(cl_mem),&(dc->d_limit_tensor));
  checkCL(errCode, !=, CL_SUCCESS, "faile to create kernel " ,FreeKFP);

  goto Succ;

 FreeKFC:
  clReleaseKernel(dc->k_face_colors);
 FreeKFP:
  clReleaseKernel(dc->k_face_points);
 FreeKEP:
  clReleaseKernel(dc->k_edge_points);
 FreeKL:
  clReleaseKernel(dc->k_limitf);
 FreeKS:
  clReleaseKernel(dc->k_scalef);
 FreeDFC:
  clReleaseMemObject(dc->d_face_colors);
 FreeDFP:
  clReleaseMemObject(dc->d_face_points);
 FreeDEP:
  clReleaseMemObject(dc->d_edge_points);
 FreeDLT:
  clReleaseMemObject(dc->d_limit_tensor);
 FreeDVT:
  clReleaseMemObject(dc->d_voxel_tensor);
 FreeDC:
  free(dc);
 Fail:
  dc = NULL;
 Succ:
  return dc;
}

void release_dev_context(struct device_contexts* dc) {
  clReleaseKernel(dc->k_face_colors);
  clReleaseKernel(dc->k_face_points);
  clReleaseKernel(dc->k_edge_points);
  clReleaseKernel(dc->k_limitf);
  clReleaseKernel(dc->k_scalef);
  clReleaseMemObject(dc->d_face_colors);
  clReleaseMemObject(dc->d_face_points);
  clReleaseMemObject(dc->d_edge_points);
  clReleaseMemObject(dc->d_limit_tensor);
  clReleaseMemObject(dc->d_voxel_tensor);
  free(dc);
}


cl_int add_scale_computing(struct computing_context* cc, struct device_contexts* dc) {
  assert(cc != NULL);
  assert(dc != NULL);
  size_t ws[] = {cc -> width * cc -> height * cc -> depth};
  return clEnqueueNDRangeKernel(get_global_command_queue()[dc->dev_idx],
                                dc -> k_scalef, 1, 0, ws,
                                0,0,0,0);
}

cl_int add_limit_computing(struct computing_context* cc, struct device_contexts* dc) {
  assert(cc != NULL);
  assert(dc != NULL);
  size_t ws[] = {cc -> width * cc -> height * cc -> depth};
  return clEnqueueNDRangeKernel(get_global_command_queue()[dc->dev_idx],
                                dc -> k_limitf, 1, 0, ws,
                                0,0,0,0);
}


cl_int add_edgeps_computing(struct computing_context* cc, struct device_contexts* dc) {
  assert(cc != NULL);
  assert(dc != NULL);
  size_t ws[] = {cc -> width, cc -> height, cc -> depth};
  return clEnqueueNDRangeKernel(get_global_command_queue()[dc->dev_idx],
                                dc -> k_edge_points, 3, 0, ws,
                                0,0,0,0);
}

cl_int add_faceps_computing(struct computing_context* cc, struct device_contexts* dc) {
  assert(cc != NULL);
  assert(dc != NULL);
  size_t ws[] = {cc -> width, cc -> height, cc -> depth};
  return clEnqueueNDRangeKernel(get_global_command_queue()[dc->dev_idx],
                                dc -> k_face_points, 3, 0, ws,
                                0,0,0,0);
}

cl_int add_color_computing(struct computing_context* cc, struct device_contexts* dc) {
  assert(cc != NULL);
  assert(dc != NULL);
  size_t ws[] = {cc -> width, cc -> height, cc -> depth};
  return clEnqueueNDRangeKernel(get_global_command_queue()[dc->dev_idx],
                                dc -> k_face_colors, 3, 0, ws,
                                0,0,0,0);
}

cl_int sync_computing(struct computing_context* cc, struct device_contexts* dc) {
  assert(cc != NULL);
  assert(dc != NULL);
  return clFinish(get_global_command_queue()[dc->dev_idx]);
}

#define ccsize(cc) (cc->depth * cc->width * cc->height)

cl_int copy_memory_voxel_tensor(struct computing_context* cc, struct device_contexts* dc) {
  assert(cc != NULL);
  assert(dc != NULL);
  return clEnqueueWriteBuffer(get_global_command_queue()[dc->dev_idx],
                             dc->d_voxel_tensor, CL_TRUE,
                             0, sizeof(float)*ccsize(cc), cc->voxel_tensor,
                             0,0,0);
}

cl_int copy_memory_edge_points(struct computing_context* cc, struct device_contexts* dc) {
  assert(cc != NULL);
  assert(dc != NULL);
  return clEnqueueReadBuffer(get_global_command_queue()[dc->dev_idx],
                             dc->d_edge_points, CL_TRUE,
                             0, sizeof(float)*ccsize(cc)*72, cc->edge_points,
                             0,0,0);
}

cl_int copy_memory_face_points(struct computing_context* cc, struct device_contexts* dc) {
  assert(cc != NULL);
  assert(dc != NULL);
  return clEnqueueReadBuffer(get_global_command_queue()[dc->dev_idx],
                             dc->d_face_points, CL_TRUE,
                             0, sizeof(float)*ccsize(cc)*72, cc->face_points,
                             0,0,0);
}

cl_int copy_memory_face_colors(struct computing_context* cc, struct device_contexts* dc) {
  assert(cc != NULL);
  assert(dc != NULL);
  return clEnqueueReadBuffer(get_global_command_queue()[dc->dev_idx],
                             dc->d_face_colors, CL_TRUE,
                             0, sizeof(float)*ccsize(cc)*3, cc->face_colors,
                             0,0,0);
}
#undef ccsize
