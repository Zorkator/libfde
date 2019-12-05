cmake_minimum_required(VERSION 2.8.12)
project(fortres C CXX)

add_library(fortres
  ./dirent.c
  ./fort_dirent.cpp
  ./fort_exception.cpp
  ./fort_plugin.cpp
  ./fort_tracestack.cpp
)

target_include_directories(fortres PUBLIC include)
target_include_directories(fortres PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

include(CheckIncludeFiles)
include(CheckFunctionExists)
include(CheckSymbolExists)
include(CheckCSourceCompiles)

check_include_files("windows.h" HAVE_WINDOWS_H)
if(HAVE_WINDOWS_H)
  check_symbol_exists("strtok_s" "string.h" HAVE_STRING_SECURE)
  check_include_files("windows.h;DbgHelp.h" HAVE_DBGHELP_H)
  check_include_files("windows.h;libloaderapi.h" HAVE_LIBLOADERAPI_H)
endif()

check_include_files("dirent.h" HAVE_DIRENT_H)
check_include_files("dlfcn.h" HAVE_DLFCN_H)

if(HAVE_DBGHELP_H)
  target_link_libraries(fortres PRIVATE Dbghelp)
endif()

if(HAVE_DIRENT_H)
  set_source_files_properties(./dirent.c PROPERTIES HEADER_FILE_ONLY YES)
endif()

if(HAVE_DLFCN_H)
  target_link_libraries(fortres PRIVATE dl)
endif()

#file(TO_NATIVE_PATH "/" LIB_DIR_SEP)
set(PATH_SEP "/" CACHE STRING "")
set(PATH_SEP_OTHER "\\\\" CACHE STRING "")

if(WIN32)
  set(LIB_PATH_VAR "PATH" CACHE STRING "")
  set(LIB_PATH_SEP ";" CACHE STRING "")
else()
  set(LIB_PATH_VAR "LD_LIBRARY_PATH" CACHE STRING "")
  set(LIB_PATH_SEP ":" CACHE STRING "")
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.in ${CMAKE_CURRENT_BINARY_DIR}/config.h)

