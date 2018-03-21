#include <gtest/gtest.h>
#include <vts.h>
#include <stdint.h>
#include <cstdlib>
#include <ctime>
#include <iostream>


#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif // __APPLE__


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

// Tesing for casting
class CastingTest : public testing::Test{
public:
  cl_mem ptr_u8 = 0;
  cl_mem ptr_f = 0;
  cl_mem out_u8 = 0;
  cl_mem out_f = 0;
  uint8_t* std_u8 = 0;
  float* std_f = 0;
  cl_kernel cast8f_call = 0;
  cl_kernel castf8_call = 0;

  virtual void SetUp() {
    cl_int errCode = CL_SUCCESS;
    std_u8 = new uint8_t[13];
    std_f  = new float[13];

	for (auto i = 0; i < 13; ++i) {
		std_f[i] = i;
		std_u8[i] = i;
	}

    ptr_u8 = clCreateBuffer(get_global_context(),
                            CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                            sizeof(uint8_t) * 13,
                            std_u8, &errCode);
    if(errCode != CL_SUCCESS){
      std::cerr << "fail to create u8 cl" << std::endl;
      goto DeletePtr;
    }

    ptr_f = clCreateBuffer(get_global_context(),
                           CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                           sizeof(float) * 13,
                           std_f, &errCode);
    if(errCode != CL_SUCCESS){
      std::cerr << "fail to create f cl" << std::endl;
      goto FreeU8CL;
    }

    out_u8 = clCreateBuffer(get_global_context(),
                            CL_MEM_WRITE_ONLY,
                            sizeof(uint8_t) * 13,
                            nullptr, &errCode);
    if(errCode != CL_SUCCESS){
      std::cerr << "fail to create u8 out" << std::endl;
      goto FreeFCL;
    }

    out_f = clCreateBuffer(get_global_context(),
                           CL_MEM_WRITE_ONLY,
                           sizeof(float) * 13,
                           nullptr, &errCode);
    if(errCode != CL_SUCCESS){
      std::cerr << "fail to create f out" << std::endl;
      goto FreeU8OUT;
    }
	for (auto i = 0; i < 13; ++i) {
		std_f[i]  = 64;
		std_u8[i] = 64;
	}

    goto Success;

  FreeFOUT:
    clReleaseMemObject(out_f);
  FreeU8OUT:
    clReleaseMemObject(out_u8);
  FreeFCL:
    clReleaseMemObject(ptr_f);
  FreeU8CL:
    clReleaseMemObject(ptr_u8);
  DeletePtr:
    delete[] std_u8;
    delete[] std_f;
  Throw:
    throw "can not init";
  Success:
    return;
  }
  virtual void TearDown() {
    if(out_f)  clReleaseMemObject(out_f);
    if(out_u8) clReleaseMemObject(out_u8);
    if(ptr_f)  clReleaseMemObject(ptr_f);
    if(ptr_u8) clReleaseMemObject(ptr_u8);
    if(cast8f_call) clReleaseKernel(cast8f_call);
    if(castf8_call) clReleaseKernel(castf8_call);
    delete[] std_u8;
    delete[] std_f;
  }
};

TEST_F(CastingTest, case1) {
  size_t work_size  = 13;
  cl_int errCode;

  cast8f_call = callable_kernel("cast_u8_f",&errCode,2,
                                sizeof(cl_mem), &out_f,
                                sizeof(cl_mem), &ptr_u8);
  EXPECT_EQ(errCode, CL_SUCCESS);

  EXPECT_EQ(clEnqueueNDRangeKernel(get_global_command_queue()[0],cast8f_call,
                                   1,0,&work_size,0,0,0,0),
            CL_SUCCESS);

  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);

  EXPECT_EQ(clEnqueueReadBuffer(get_global_command_queue()[0],
                                out_f, CL_TRUE, 0, sizeof(float) * 13, std_f, 0, 0, 0),
            CL_SUCCESS);

  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);


  castf8_call = callable_kernel("cast_f_u8",&errCode,2,
                                sizeof(cl_mem), &out_u8,
                                sizeof(cl_mem), &ptr_f);
  EXPECT_EQ(errCode, CL_SUCCESS);

  EXPECT_EQ(clEnqueueNDRangeKernel(get_global_command_queue()[0],castf8_call,
                                   1,0,&work_size,0,0,0,0),
            CL_SUCCESS);

  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);

  EXPECT_EQ(clEnqueueReadBuffer(get_global_command_queue()[0],
                                out_u8, CL_TRUE, 0, sizeof(uint8_t) * 13, std_u8, 0, 0, 0),
            CL_SUCCESS);

  EXPECT_EQ(clFinish(get_global_command_queue()[0]),CL_SUCCESS);

  for(auto i = 0; i < 13; ++i) {
    EXPECT_EQ(std_u8[i],(uint8_t)i);
  }

  for (auto i = 0; i < 13; ++i) {
	  EXPECT_EQ(std_f[i], (float)i);
  }
}
