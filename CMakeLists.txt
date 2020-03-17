cmake_minimum_required(VERSION 3.8)
project(fde Fortran CXX)

# Set name and version.
set(FDE_NAME "fde")
set(FDE_VERSION_SO 1)
set(FDE_VERSION_MAJOR 2)
set(FDE_VERSION_MINOR 7)
set(FDE_VERSION_PATCH 3)
set(FDE_VERSION       "${FDE_VERSION_MAJOR}.${FDE_VERSION_MINOR}.${FDE_VERSION_PATCH}")

set(CMI_DOWNLOAD_TAG 282a4869b253d3a52ca4a57c70124eae0af7873a)
include(cmi.cmake)
cmi_set_build_environment()

# Generating files
cmi_find_python(PACKAGES docopt)

# fortres
add_subdirectory(libfortres)
add_subdirectory(src)

# tests
if(BUILD_TESTING)
  add_subdirectory(test)
endif()
