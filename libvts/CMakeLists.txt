# libvts CMake file

## CMake 3.3
cmake_minimum_required(VERSION 3.3)

project(
  libvts
  VERSION 0.1.1
  )

## Flags
option(ENABLE_TEST "Enable test" ON)
option(DOWNLOAD_GTEST "Download google test" ON)

## setting up the c++-11 and c-11
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_C_STANDARD   11)

if(${MSVC})
  set (CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
endif(${MSVC})

## OpenCL
find_package(OpenCL 1.0 REQUIRED)

## includes
include_directories(${PROJECT_SOURCE_DIR}/include)
include_directories(${OpenCL_INCLUDE_DIRS})
include_directories(${PROJECT_SOURCE_DIR})

## sources
set(VTS_SRC_DIR  ${PROJECT_SOURCE_DIR}/source)
set(
  VTS_SRC_FILES
  ${VTS_SRC_DIR}/vts.cc
  ${VTS_SRC_DIR}/internal.cc
  ${VTS_SRC_DIR}/kernels.cc
  )
## targets
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
  LIBRARY  DESTINATION ${LIB_INS_PATH} COMPONENT runtime
  PUBLIC_HEADER DESTINATION include COMPONENT dev
  )

## cpack
SET(CPACK_DEB_COMPONENT_INSTALL ON)
set(CPACK_DEBIAN_PACKAGE_SOURCE "libvts")
set(CPACK_GENERATOR "DEB;TGZ")
set(CPACK_PACKAGE_CONTACT "Johann Lee <me@qinka.pro>")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6 (>= 2.3.1-6), llibgcc1 (>= 1:3.4.2-12), libstdc++6 (>= 5)")
set(CPACK_DEBIAN_DEV_PACKAGE_DEPENDS "${CPACK_DEBIAN_PACKAGE_DEPENDS}, libvts-runtime (>= 0.1.1)")
set(CPACK_DEBIAN_RUNETIME_PACKAGE_SECTION "runtime")
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION_SUMMARY "Library voxel tensors, libvts for short.")
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION "The voxel tensor library for voxel tensor preview.")
include(CPack)

if(ENABLE_TEST)
  enable_testing()
  ## GoogleTest
  if(DOWNLOAD_GTEST)
    configure_file(gtest.cmake.in googletest-download/CMakeLists.txt)
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
  target_link_libraries(vts-test gtest_main vts ${OpenCL_LIBRARY})
  if(NOT(CMAKE_SYSTEM_NAME MATCHES "Windows"))
    target_link_libraries(vts-test pthread)
  endif(NOT(CMAKE_SYSTEM_NAME MATCHES "Windows"))
  add_test(LIBVTS_TEST vts-test)
endif()
