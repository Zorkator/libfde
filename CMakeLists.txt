cmake_minimum_required(VERSION 3.8)
project(fde Fortran CXX)

# Set name and version.
set(FDE_NAME "fde")
set(FDE_VERSION_SO 1)
set(FDE_VERSION_MAJOR 2)
set(FDE_VERSION_MINOR 7)
set(FDE_VERSION_PATCH 3)
set(FDE_VERSION       "${FDE_VERSION_MAJOR}.${FDE_VERSION_MINOR}.${FDE_VERSION_PATCH}")

set(CMI_DOWNLOAD_TAG 364b54e0200459f446190a94d177d27317d9447b)
include(cmi.cmake)
cmi_set_build_environment()

# Generating files
cmi_find_python()

# fortres
add_subdirectory(libfortres)
add_subdirectory(src)

# tests
if(BUILD_TESTING)
  add_subdirectory(test)
endif()
