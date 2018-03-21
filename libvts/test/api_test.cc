#include <gtest/gtest.h>
#include <vts.h>
#include <stdint.h>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <exception>
#include <cstdarg>
#include <functional>

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif // __APPLE__


// constant

constexpr int CastingTestSize = 13;

extern char const cast_u8_f[]   = "cast_u8_f";
extern char const cast_f_u8[]   = "cast_f_u8";
extern char const scalef[]      = "scalef";
extern char const limitf[]      = "limitf";
extern char const edge_points[] = "edge_points";
extern char const face_points[] = "face_points" ;


// testing fixture and environment fixture
class ApiEnvironment : public testing::Environment{
public:
  virtual ~ApiEnvironment() {
    std::cerr << "ApiEnvironment deconstructed." << std::endl;
  }
  virtual void SetUp() {
    load_library_context(0);
  }
  virtual void TearDown() {
    free_library_context();
  }
};

testing::Environment* const global_env = testing::AddGlobalTestEnvironment(new ApiEnvironment);

// Testing for test_cl_kernel_call

TEST(test_kernel_test, _1st) {
  test_cl_kernel_call(4);
}


TEST(test_kernel_test, _2nd) {
  test_cl_kernel_call(4);
}

TEST(test_kernel_test, _3rd) {
  test_cl_kernel_call(10);
}

TEST(test_kernel_test, _4th) {
  test_cl_kernel_call(10);
}

TEST(print_all_plat_n_dev_test,test1) {
  print_all_plat_n_dev();
}


template<typename T>
class MemoryGen {
  cl_mem _mem = 0;
  size_t _num;
public:
  MemoryGen(size_t num, cl_mem_flags flags, T *ptr) {
    cl_int errCode;
    _num = num;
    _mem = clCreateBuffer(get_global_context(),
                          flags,
                          sizeof(T) * num,
                          ptr, &errCode);
    if(errCode != CL_SUCCESS){
      std::cerr << "fail to create memory " << __LINE__ << std ::endl;
      throw std::exception("Fail to create memory");
    }
  }
  ~MemoryGen() {
    if(_mem) clReleaseMemObject(_mem);
  }
  cl_mem* getMemObj () {
    return &_mem;
  }
  cl_int copyOut(void* ptr) {
    cl_int errCode = CL_SUCCESS;
    errCode = clEnqueueReadBuffer(get_global_command_queue()[0],
                                  _mem, CL_TRUE, 0, sizeof(T) * _num, ptr, 0, 0, 0);
    if (errCode != CL_SUCCESS) {
      std::cerr << "fail to copy " << __LINE__ << std::endl;
      return errCode;
    }

    errCode = clFinish(get_global_command_queue()[0]);
    if (errCode != CL_SUCCESS) {
      std::cerr << "fail to copy " << __LINE__ << std::endl;
      return errCode;
    }
    return errCode;
  }
};

template<char const *kn>
class KernelGen {
  const char * kernel_name;
  cl_kernel _kernel = 0;
public:
  KernelGen(size_t argc,...) {
    kernel_name = kn;
    cl_int errCode = CL_SUCCESS;
    va_list ap;
    va_start(ap,argc);
    _kernel = callable_kernel_args(kernel_name,&errCode,argc,ap);
    va_end(ap);
    if(errCode != CL_SUCCESS) {
      std::cerr << "fail to create kernel" << __LINE__ << std::endl;
      throw std::exception("Fail to create kernel");
    }
  }
  ~KernelGen() {
    clReleaseKernel(_kernel);
  }
  cl_int callKernel(cl_uint dim, const size_t *gws) {
    return clEnqueueNDRangeKernel(get_global_command_queue()[0],_kernel,
                                  dim,0,gws,0,0,0,0);
  }
};

// testing helper for y=f(x)
template<typename T1, typename T2, int inSize, int outSize>
class FxTest {
public:
  FxTest() {
    from = new MemoryGen<T1>(inSize,  CL_MEM_READ_WRITE, nullptr);
    to   = new MemoryGen<T2>(outSize, CL_MEM_READ_WRITE, nullptr);
    origin = new T1[inSize];
    result = new T2[outSize];
  }

  ~FxTest() {
    if(origin) delete[] origin;
    if(result) delete[] result;
    if(from)   delete   from;
    if(to)     delete   to;
  }

  MemoryGen<T1> * from   = nullptr;
  MemoryGen<T2> * to     = nullptr;
  T1            * origin = nullptr;
  T2            * result = nullptr;
  void rands() {
    srand (time(NULL));
    for(auto i = 0; i < inSize; ++i){
      origin[i] = static_cast <T1> (rand()) / static_cast <T1> (RAND_MAX);
    }
  }
  void map(std::function<T1(T1,int)> func){
    srand (time(NULL));
    for(auto i = 0; i < inSize; ++i){
      origin[i] = func(origin[i],rand());
    }
  }
  cl_int write() {
    return clEnqueueWriteBuffer(get_global_command_queue()[0], *(from->getMemObj()), CL_TRUE,
                                0, sizeof(T1)*inSize, origin,
                                0, 0, 0);
  }
  cl_int read() {
    return clEnqueueReadBuffer(get_global_command_queue()[0], *(to->getMemObj()), CL_TRUE,
                               0, sizeof(T2)*outSize, result,
                               0, 0, 0);
  }
};

TEST(cast,u8f) {
  auto on = FxTest<uint8_t,float,13,13>();
  size_t ws = 13;

  on.rands();
  EXPECT_EQ(on.write(), CL_SUCCESS);
  float scal = 2;
  auto scalef_call =
    KernelGen<cast_u8_f>
    (2,
     sizeof(cl_mem), on.to->getMemObj(),
     sizeof(cl_mem), on.from->getMemObj());
  EXPECT_EQ(scalef_call.callKernel(1,&ws), CL_SUCCESS);
  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);
  EXPECT_EQ(on.read(), CL_SUCCESS);
  for(auto i = 0; i < 13; ++i){
    EXPECT_EQ(on.result[i], (float)on.origin[i]);
  }
}


TEST(cast,fu8) {
  auto on = FxTest<float, uint8_t,13,13>();
  size_t ws = 13;

  on.rands();
  EXPECT_EQ(on.write(), CL_SUCCESS);
  float scal = 2;
  auto scalef_call =
    KernelGen<cast_f_u8>
    (2,
     sizeof(cl_mem), on.to->getMemObj(),
     sizeof(cl_mem), on.from->getMemObj());
  EXPECT_EQ(scalef_call.callKernel(1,&ws), CL_SUCCESS);
  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);
  EXPECT_EQ(on.read(), CL_SUCCESS);
  for(auto i = 0; i < 13; ++i){
    EXPECT_EQ(on.result[i], (uint8_t)on.origin[i]);
  }
}


TEST(scale,scalef1) {
  auto on = FxTest<float,float,13,13>();
  size_t ws = 13;

  on.rands();
  EXPECT_EQ(on.write(), CL_SUCCESS);
  float scal = 2;
  auto scalef_call =
    KernelGen<scalef>
    (3,
     sizeof(cl_mem), on.to->getMemObj(),
     sizeof(cl_mem), on.from->getMemObj(),
     sizeof(float), &scal);
  EXPECT_EQ(scalef_call.callKernel(1,&ws), CL_SUCCESS);
  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);
  EXPECT_EQ(on.read(), CL_SUCCESS);
  for(auto i = 0; i < 13; ++i){
    EXPECT_EQ(on.result[i], scal * on.origin[i]);
  }
}

TEST(scale,limitf2) {
  auto on = FxTest<float,float,13,13>();
  size_t ws = 13;

  on.rands();
  EXPECT_EQ(on.write(), CL_SUCCESS);
  float top    = 0.25;
  float bottom = 0.75;
  auto limitf_call =
    KernelGen<limitf>
    (4,
     sizeof(cl_mem), on.to->getMemObj(),
     sizeof(cl_mem), on.from->getMemObj(),
     sizeof(float), &top,
     sizeof(float), &bottom);
  EXPECT_EQ(limitf_call.callKernel(1,&ws), CL_SUCCESS);
  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);
  EXPECT_EQ(on.read(), CL_SUCCESS);
  auto func = [&](float a){
    if (a < top && a > bottom)
      return a;
    else
      return (float)0;
  };
  for(auto i = 0; i < 13; ++i){
    EXPECT_EQ(on.result[i], func(on.origin[i]));
  }
}



float edgeList[] = {
  0,  0,  0,  1,  0,  0,
  1,  0,  0,  1,  1,  0,
  1,  1,  0,  1,  1,  1,

  1,  0,  0,  1,  0,  1,
  1,  0,  1,  1,  1,  1,
  1,  1,  1,  0,  1,  1,

  1,  0,  1,  0,  0,  1,
  0,  0,  1,  0,  1,  1,
  0,  1,  1,  0,  1,  0,

  0,  0,  1,  0,  0,  0,
  0,  0,  0,  0,  1,  0,
  0,  1,  0,  1,  1,  0,
};

TEST(draw,edge1) {
  auto on = FxTest<float,float,1,1*72>();
  size_t ws[] ={1,1,1};

  on.rands();
  on.map([](float a, int b){
      std::cout << b % 2 << std::endl;
      if(b % 2)
        return a;
      else
        return (float)0;
    });
  EXPECT_EQ(on.write(), CL_SUCCESS);
  auto called_kernel =
    KernelGen<edge_points>
    (2,
     sizeof(cl_mem), on.to->getMemObj(),
     sizeof(cl_mem), on.from->getMemObj());
  EXPECT_EQ(called_kernel.callKernel(3,ws), CL_SUCCESS);
  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);
  EXPECT_EQ(on.read(), CL_SUCCESS);

  for(auto x = 0; x < 1; ++x)
    for(auto y = 0; y < 1; ++y)
      for(auto z = 0; z < 1; ++z) {
        int i = x + y * 1 + z * 1 * 1;
        float *cur = on.result + i * 72;
        if (on.origin[i] > 0)
          for (auto j = 0; j < 24; ++j){
            EXPECT_EQ(cur[j*3+0], edgeList[j*3+0] + x);
            EXPECT_EQ(cur[j*3+1], edgeList[j*3+1] + y);
            EXPECT_EQ(cur[j*3+2], edgeList[j*3+2] + z);
          }
        else
          for (auto j = 0; j < 72; ++j){
            EXPECT_EQ(cur[j], 0);
          }
      }
}



TEST(draw,edge2) {
  auto on = FxTest<float,float,8,8*72>();
  size_t ws[] = {2,2,2};

  on.rands();
  on.map([](float a, int b){
      std::cout << b % 2 << std::endl;
      if(b % 2)
        return a;
      else
        return (float)0;
    });
  EXPECT_EQ(on.write(), CL_SUCCESS);
  auto called_kernel =
    KernelGen<edge_points>
    (2,
     sizeof(cl_mem), on.to->getMemObj(),
     sizeof(cl_mem), on.from->getMemObj());
  EXPECT_EQ(called_kernel.callKernel(3,ws), CL_SUCCESS);
  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);
  EXPECT_EQ(on.read(), CL_SUCCESS);

  for(auto x = 0; x < 2; ++x)
    for(auto y = 0; y < 2; ++y)
      for(auto z = 0; z < 2; ++z) {
        int i = x + y * 2 + z * 2 * 2;
        float *cur = on.result + i * 72;
        if (on.origin[i] > 0)
          for (auto j = 0; j < 24; ++j){
            EXPECT_EQ(cur[j*3+0], edgeList[j*3+0] + x);
            EXPECT_EQ(cur[j*3+1], edgeList[j*3+1] + y);
            EXPECT_EQ(cur[j*3+2], edgeList[j*3+2] + z);
          }
        else
          for (auto j = 0; j < 72; ++j){
            EXPECT_EQ(cur[j], 0);
          }
      }
}




const float faceList[] = {
  0,  0,  0,  1,  0,  0,
  1,  1,  0,  0,  1,  0,

  0,  0,  0,  0,  1,  0,
  0,  1,  1,  0,  0,  1,

  0,  0,  0,  1,  0,  0,
  1,  0,  1,  0,  0,  1,


  1,  1,  1,  0,  1,  1,
  0,  0,  1,  1,  0,  1,

  1,  1,  1,  1,  0,  1,
  1,  0,  0,  1,  1,  0,

  1,  1,  1,  0,  1,  1,
  0,  1,  0,  1,  1,  0,
};

TEST(draw,face1) {
  auto on = FxTest<float,float,1,1*72>();
  size_t ws[] ={1,1,1};

  on.rands();
  on.map([](float a, int b){
      std::cout << b % 2 << std::endl;
      if(b % 2)
        return a;
      else
        return (float)0;
    });
  EXPECT_EQ(on.write(), CL_SUCCESS);
  auto called_kernel =
    KernelGen<face_points>
    (2,
     sizeof(cl_mem), on.to->getMemObj(),
     sizeof(cl_mem), on.from->getMemObj());
  EXPECT_EQ(called_kernel.callKernel(3,ws), CL_SUCCESS);
  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);
  EXPECT_EQ(on.read(), CL_SUCCESS);

  for(auto x = 0; x < 1; ++x)
    for(auto y = 0; y < 1; ++y)
      for(auto z = 0; z < 1; ++z) {
        int i = x + y * 1 + z * 1 * 1;
        float *cur = on.result + i * 72;
        if (on.origin[i] > 0)
          for (auto j = 0; j < 24; ++j){
            EXPECT_EQ(cur[j*3+0], faceList[j*3+0] + x);
            EXPECT_EQ(cur[j*3+1], faceList[j*3+1] + y);
            EXPECT_EQ(cur[j*3+2], faceList[j*3+2] + z);
          }
        else
          for (auto j = 0; j < 72; ++j){
            EXPECT_EQ(cur[j], 0);
          }
      }
}


TEST(draw,face2) {
  auto on = FxTest<float,float,8,8*72>();
  size_t ws[] = {2,2,2};

  on.rands();
  on.map([](float a, int b){
      std::cout << b % 2 << std::endl;
      if(b % 2)
        return a;
      else
        return (float)0;
    });
  EXPECT_EQ(on.write(), CL_SUCCESS);
  auto called_kernel =
    KernelGen<face_points>
    (2,
     sizeof(cl_mem), on.to->getMemObj(),
     sizeof(cl_mem), on.from->getMemObj());
  EXPECT_EQ(called_kernel.callKernel(3,ws), CL_SUCCESS);
  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);
  EXPECT_EQ(on.read(), CL_SUCCESS);
  
  for(auto x = 0; x < 2; ++x)
    for(auto y = 0; y < 2; ++y)
      for(auto z = 0; z < 2; ++z) {
        int i = x + y * 2 + z * 2 * 2;
        float *cur = on.result + i * 72;
        if (on.origin[i] > 0)
          for (auto j = 0; j < 24; ++j){
            EXPECT_EQ(cur[j*3+0], faceList[j*3+0] + x);
            EXPECT_EQ(cur[j*3+1], faceList[j*3+1] + y);
            EXPECT_EQ(cur[j*3+2], faceList[j*3+2] + z);
          }
        else
          for (auto j = 0; j < 72; ++j){
            EXPECT_EQ(cur[j], 0);
          }
      }
}
