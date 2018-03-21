#include <vts-view.h>

computing_reg_f* create_crf(size_t depth, size_t width, size_t height) {

  const size_t num = depth * width * height;
  computing_reg_f* ptr = NULL;
  float*            vt = NULL;
  float*            lt = NULL;
  float*            fp = NULL;
  float*            ep = NULL;
  float*            fc = NULL;

  ptr =  malloc(sizeof(computing_reg_f));
  if (!ptr) goto Fail;

  vt = malloc(sizeof(float) * num);
  if(!vt) goto FreeCRF;

  lt = malloc(sizeof(float) * num);
  if(!lt) goto FreeVT;

  ep = malloc(sizeof(float) * num * 72);
  if(!ep) goto FreeLT;

  fp = malloc(sizeof(float) * sum * 72);
  if(!fp) goto FreeEP;

  fc = malloc(sizeof(float) * sum * 72);
  if(!fc) goto FreeFP;

  ptr = {
    .depth        = depth;
    .width        = width;
    .height       = height;
    .scal         = 1;
    .bottom       = 0;
    .top          = 1.1;
    .voxel_tensor = vt;
    .edge_points  = ep;
    .face_points  = fp;
    .face_colors  = fc;
  };

  goto Succ;

 FreeFP:
  free(fp);
 FreeEP:
  free(ep);
 FreeLT:
  free(lt);
 FreeVT:
  free(vt);
 FreeCRF:
  free(ptr);
 Fail:
  ptr = NULL;
 Succ:
  return ptr;
}

void release_crf(computing_reg_f * crf) {
  free(crf -> voxel_tensor);
  free(crt -> limit_tensor);
  free(crf -> edge_points);
  free(crf -> face_points);
  free(crf -> face_colors);
  free(crf);
}
