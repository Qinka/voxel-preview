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

extern char const cast_u8_f[] = "cast_u8_f";
extern char const cast_f_u8[] = "cast_f_u8";
extern char const scalef[]    = "scalef";
extern char const limitf[]    = "limitf";


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

template<char const *kn, size_t ws>
class KernelGen {
  const char * kernel_name;
  cl_kernel _kernel = 0;
  size_t work_size = ws;
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
  cl_int callKernel() {
    return clEnqueueNDRangeKernel(get_global_command_queue()[0],_kernel,
                                  1,0,&work_size,0,0,0,0);
  }
};

// Tesing for casting
class CastingTest : public testing::Test{
public:
  MemoryGen<uint8_t> * ptr_u8      = nullptr;
  MemoryGen<float>   * ptr_f       = nullptr;
  MemoryGen<uint8_t> * out_u8      = nullptr;
  MemoryGen<float>   * out_f       = nullptr;
  uint8_t            * std_u8      = nullptr;
  float              * std_f       = nullptr;

  virtual void SetUp() {
    try {
      std_u8 = new uint8_t[CastingTestSize];
      std_f  = new float[CastingTestSize];

      for (auto i = 0; i < CastingTestSize; ++i) {
        std_f[i] = i;
        std_u8[i] = i;
      }

      ptr_f  = new MemoryGen<float>(CastingTestSize,
                                    CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                                    std_f);
      ptr_u8 = new MemoryGen<uint8_t>(CastingTestSize,
                                      CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                                      std_u8);
      out_f  = new MemoryGen<float>(CastingTestSize,
                                    CL_MEM_WRITE_ONLY,
                                    nullptr);
      out_u8 = new MemoryGen<uint8_t>(CastingTestSize,
                                      CL_MEM_WRITE_ONLY,
                                      nullptr);
      for (auto i = 0; i < 13; ++i) {
        std_f[i]  = 64;
        std_u8[i] = 64;
      }
    }
    catch (std::exception e) {
      if(ptr_f)  delete   ptr_f;
      if(ptr_u8) delete   ptr_u8;
      if(out_f)  delete   out_f;
      if(out_u8) delete   out_u8;
      if(std_u8) delete[] std_u8;
      if(std_f)  delete[] std_f;
      throw e;
    }
  }
  virtual void TearDown() {
    if(ptr_f)  delete   ptr_f;
    if(ptr_u8) delete   ptr_u8;
    if(out_f)  delete   out_f;
    if(out_u8) delete   out_u8;
    if(std_u8) delete[] std_u8;
    if(std_f)  delete[] std_f;
  }
};

TEST_F(CastingTest, case1) {
  size_t work_size  = 13;
  cl_int errCode;

  auto cast8f_call =
    KernelGen<cast_u8_f,CastingTestSize>
    (2,
     sizeof(cl_mem), out_f->getMemObj(),
     sizeof(cl_mem), ptr_u8->getMemObj());

  auto castf8_call =
    KernelGen<cast_f_u8,CastingTestSize>
    (2,
     sizeof(cl_mem), out_u8->getMemObj(),
     sizeof(cl_mem), ptr_f->getMemObj());



  EXPECT_EQ(castf8_call.callKernel(), CL_SUCCESS);

  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);

  EXPECT_EQ(out_u8->copyOut(std_u8), CL_SUCCESS);

  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);

  EXPECT_EQ(cast8f_call.callKernel(), CL_SUCCESS);

  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);

  EXPECT_EQ(out_f->copyOut(std_f), CL_SUCCESS);

  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);

  for(auto i = 0; i < 13; ++i) {
    EXPECT_EQ(std_u8[i],(uint8_t)i);
  }

  for (auto i = 0; i < 13; ++i) {
	  EXPECT_EQ(std_f[i], (float)i);
  }
}


template<typename T1, typename T2, int FxTestSize>
class FxTest {
public:
  FxTest() {
    from = new MemoryGen<T1>(FxTestSize, CL_MEM_READ_WRITE, nullptr);
    to   = new MemoryGen<T2>(FxTestSize, CL_MEM_READ_WRITE, nullptr);
    origin = new T1[FxTestSize];
    result = new T2[FxTestSize];
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
    for(auto i = 0; i < FxTestSize; ++i){
      origin[i] = static_cast <T1> (rand()) / static_cast <T1> (RAND_MAX);
    }
  }
  cl_int write() {
    return clEnqueueWriteBuffer(get_global_command_queue()[0], *(from->getMemObj()), CL_TRUE,
                                0, sizeof(T1)*FxTestSize, origin,
                                0, 0, 0);
  }
  cl_int read() {
    return clEnqueueReadBuffer(get_global_command_queue()[0], *(to->getMemObj()), CL_TRUE,
                               0, sizeof(T2)*FxTestSize, result,
                               0, 0, 0);
  }
};

TEST(scale,scalef1) {
  auto on = FxTest<float,float,13>();

  on.rands();
  EXPECT_EQ(on.write(), CL_SUCCESS);
  float scal = 2;
  auto scalef_call =
    KernelGen<scalef,13>
    (3,
     sizeof(cl_mem), on.to->getMemObj(),
     sizeof(cl_mem), on.from->getMemObj(),
     sizeof(float), &scal);
  EXPECT_EQ(scalef_call.callKernel(), CL_SUCCESS);
  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);
  EXPECT_EQ(on.read(), CL_SUCCESS);
  for(auto i = 0; i < 13; ++i){
    EXPECT_EQ(on.result[i], scal * on.origin[i]);
  }
}
