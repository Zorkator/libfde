cmake_minimum_required(VERSION 2.8.12)
project(fortres C CXX)

add_library(fortres
  ./dirent.c
  ./fort_dirent.cpp
  ./fort_exception.cpp
  ./fort_plugin.cpp
  ./fort_tracestack.cpp
)

if (WIN32)
  target_link_libraries(fortres PRIVATE Dbghelp.lib)
else ()
  set_source_files_properties(dirent.c PROPERTIES HEADER_FILE_ONLY YES)
endif ()

target_include_directories(fortres PUBLIC include)
