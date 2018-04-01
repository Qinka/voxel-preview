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

enable_testing()
## GoogleTest
if(${DOWNLOAD_GTEST})
  configure_file(CMakeLists.txt.in googletest-download/CMakeLists.txt)
  execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
    RESULT_VARIABLE result
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/googletest-download )
  if(result)
    message(FATAL_ERROR "CMake step for googletest failed: ${result}")
  endif()
  execute_process(COMMAND ${CMAKE_COMMAND} --build .
    RESULT_VARIABLE result
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/googletest-download )
  if(result)
    message(FATAL_ERROR "Build step for googletest failed: ${result}")
  endif()
  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  add_subdirectory(
    ${CMAKE_BINARY_DIR}/googletest-src
    ${CMAKE_BINARY_DIR}/googletest-build
    EXCLUDE_FROM_ALL
    )
else()
  find_package(GTest REQUIRED)
endif()
include_directories(${GTEST_INCLUDE_DIRS})
add_executable(vts-test ${PROJECT_SOURCE_DIR}/test/api_test.cc)
target_link_libraries(vts-test ${GTEST_BOTH_LIBRARIES} vts ${OpenCL_LIBRARY})
if(NOT(CMAKE_SYSTEM_NAME MATCHES "Windows"))
  target_link_libraries(vts-test pthread)
endif(NOT(CMAKE_SYSTEM_NAME MATCHES "Windows"))
add_test(LIBVTS_TEST vts-test)
