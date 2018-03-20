# libvts CMake file

## CMake 3.3
cmake_minimum_required(VERSION 3.3)

project(
  libvts
  VERSION 0.1
  )


## setting up the c++-11 and c-11
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_C_STANDARD   11)

## OpenCL
find_package(OpenCL 1.0 REQUIRED)

include_directories(${PROJECT_SOURCE_DIR}/include)
include_directories(${OpenCL_INCLUDE_DIRS})
include_directories(${PROJECT_SOURCE_DIR})

set(VTS_SRC_DIR  ${PROJECT_SOURCE_DIR}/source)
set(
  VTS_SRC_FILES
  ${VTS_SRC_DIR}/vts.cc
  ${VTS_SRC_DIR}/internal.cc
  ${VTS_SRC_DIR}/kernels.cc
  )

add_library(vts SHARED ${VTS_SRC_FILES})
target_link_libraries(vts ${OpenCL_LIBRARY})
if (${MSVC})
  add_definitions( /D _VTS_EXPORT_)
else(${MSVC})
  add_definitions( -D _VTS_EXPORT_)
endif(${MSVC})

add_executable(test_exe ${PROJECT_SOURCE_DIR}/test/test.cc)
target_link_libraries(test_exe vts ${OpenCL_LIBRARY})

