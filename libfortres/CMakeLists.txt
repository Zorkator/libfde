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

include(CheckIncludeFiles)
include(CheckFunctionExists)
include(CheckSymbolExists)
include(CheckCSourceCompiles)

check_include_files("windows.h" HAVE_WINDOWS_H)
if(HAVE_WINDOWS_H)
  target_compile_definitions(fortres PRIVATE HAVE_WINDOWS_H=1)
endif()

check_include_files("windows.h;DbgHelp.h" HAVE_DBGHELP_H)
if(HAVE_DBGHELP_H)
  target_compile_definitions(fortres PRIVATE HAVE_DBGHELP_H=1)
  target_link_libraries(fortres PRIVATE Dbghelp)
endif()

check_include_files(dirent.h HAVE_DIRENT_H)
if(HAVE_DIRENT_H)
  set_source_files_properties(./dirent.c PROPERTIES HEADER_FILE_ONLY YES)
  target_compile_definitions(fortres PRIVATE HAVE_DIRENT_H=1)
endif()

check_symbol_exists("strtok_s" "string.h" HAVE_STRING_SECURE)
if(HAVE_STRING_SECURE)
  target_compile_definitions(fortres PRIVATE HAVE_STRING_SECURE=1)
endif()

check_include_files(dlfcn.h HAVE_DLFCN_H)
if(HAVE_DLFCN_H)
  target_compile_definitions(fortres PRIVATE HAVE_DLFCN_H=1)
  target_link_libraries(fortres PRIVATE dl)
endif()
