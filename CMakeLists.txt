cmake_minimum_required(VERSION 3.13)
project(fde Fortran CXX)

# Set name and version.
set(FDE_NAME "fde")
set(FDE_VERSION_MAJOR 2)
set(FDE_VERSION_MINOR 8)
set(FDE_VERSION_PATCH 8)
set(FDE_VERSION_SO "${FDE_VERSION_MAJOR}.${FDE_VERSION_MINOR}")
set(FDE_VERSION    "${FDE_VERSION_MAJOR}.${FDE_VERSION_MINOR}.${FDE_VERSION_PATCH}")

set(CMI_DOWNLOAD_TAG v0.4.12)
include(cmi.cmake)
cmi_set_build_environment()

# Generating files
cmi_find_python()

set(FDE_GENERATOR_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/typegen.py")

# fortres
add_subdirectory(libfortres)
add_subdirectory(src)

# tests
if(BUILD_TESTING)
  add_subdirectory(test)
endif()

set(RUNTIME_URL "" CACHE STRING "Git URL for runtime dependency")
set(RUNTIME_REV "" CACHE STRING "Git revision for runtime dependency")

if(NOT "${RUNTIME_URL}" STREQUAL "" OR NOT "${RUNTIME_REV}" STREQUAL "")
  if("${RUNTIME_URL}" STREQUAL "" OR "${RUNTIME_REV}" STREQUAL "")
    message(FATAL_ERROR "Both RUNTIME_URL and RUNTIME_REV must be provided to enable runtime fetch.")
  endif()
  cmi_add_git(runtime "${RUNTIME_URL}" "${RUNTIME_REV}")
  cmi_add_subdirectory(runtime)
  add_runtime()
endif()
