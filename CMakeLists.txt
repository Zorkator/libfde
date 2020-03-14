cmake_minimum_required(VERSION 3.8)
project(fde Fortran CXX)

# Set name and version.
set(FDE_NAME "fde")
set(FDE_VERSION_SO 1)
set(FDE_VERSION_MAJOR 2)
set(FDE_VERSION_MINOR 7)
set(FDE_VERSION_PATCH 3)
set(FDE_VERSION       "${FDE_VERSION_MAJOR}.${FDE_VERSION_MINOR}.${FDE_VERSION_PATCH}")

set(CMI_DOWNLOAD_TAG 9c25622e967068fc515bfccc4917149af60c047f)
include(cmi.cmake)
cmi_set_build_environment()

# fortres
add_subdirectory(libfortres)
add_subdirectory(src)

# tests
if(BUILD_TESTING)
  add_subdirectory(test)
endif()
