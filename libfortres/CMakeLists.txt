cmake_minimum_required(VERSION 2.8.12)
project(fortres C CXX)

add_library(fortres
  ./dirent.c
  ./fort_dirent.cpp
  ./fort_exception.cpp
  ./fort_plugin.cpp
  ./fort_tracestack.cpp
)

if (WIN32 OR CYGWIN)
  target_link_libraries(fortres PRIVATE Dbghelp)
else ()
  target_link_libraries(fortres PRIVATE dl)
  set_source_files_properties(./dirent.c PROPERTIES HEADER_FILE_ONLY YES)
endif ()

target_include_directories(fortres PUBLIC include)

#include(CheckIncludeFiles)
#set(CMAKE_REQUIRED_DEFINITIONS -D_CRT_SECURE_NO_WARNINGS=1)
#check_include_files(dirent.h HAVE_DIRENT_H)
#check_include_files("windows.h;DbgHelp.h" HAVE_DBGHELP_H)

#if(HAVE_DIRENT_H)
#  set_source_files_properties(./dirent.c PROPERTIES HEADER_FILE_ONLY YES)
#endif()
#if(HAVE_DBGHELP_H)
 # target_link_libraries(fortres PRIVATE Dbghelp.lib)
#endif()

