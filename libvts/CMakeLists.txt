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

if(${MSVC})
  set (CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
endif(${MSVC})

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

## Install and packing
if(CMAKE_SYSTEM_NAME MATCHES "Windows")
  set(LIB_INS_PATH bin)
else()
  set(LIB_INS_PATH lib)
endif()
set(VTS_PUB_INC ${PROJECT_SOURCE_DIR}/include/vts.h;${PROJECT_SOURCE_DIR}/include/vts_error.h)
set_target_properties(vts PROPERTIES
  PUBLIC_HEADER "${VTS_PUB_INC}"
  )

install(TARGETS vts
  ARCHIVE DESTINATION lib
  LIBRARY  DESTINATION ${LIB_INS_PATH}
  PUBLIC_HEADER DESTINATION include
  )

set(CPACK_GENERATOR "DEB")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Johann Lee")
include(CPack)

enable_testing()
## GoogleTest
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})
add_executable(vts-test ${PROJECT_SOURCE_DIR}/test/api_test.cc)
target_link_libraries(vts-test ${GTEST_BOTH_LIBRARIES} vts ${OpenCL_LIBRARY})
if(NOT(CMAKE_SYSTEM_NAME MATCHES "Windows"))
  target_link_libraries(vts-test pthread)
endif(NOT(CMAKE_SYSTEM_NAME MATCHES "Windows"))
add_test(LIBVTS_TEST vts-test)
