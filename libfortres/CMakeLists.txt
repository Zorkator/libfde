include(CheckLanguage)
enable_language(C CXX)

cmi_set_build_environment()

add_library(fortres STATIC
  ./dirent.c
  ./fort_dirent.cpp
  ./fort_exception.cpp
  ./fort_plugin.cpp
  ./fort_tracestack.cpp
)

target_include_directories(fortres PUBLIC include)
target_include_directories(fortres PRIVATE ../include)
target_include_directories(fortres PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
target_compile_features(fortres PRIVATE cxx_std_11)
set_property(TARGET fortres PROPERTY POSITION_INDEPENDENT_CODE ON)

include(CheckIncludeFiles)
include(CheckSymbolExists)

check_include_files("windows.h" HAVE_WINDOWS_H)
if(HAVE_WINDOWS_H)
  check_symbol_exists("strtok_s" "string.h" HAVE_STRING_SECURE)
  check_include_files("windows.h;DbgHelp.h" HAVE_DBGHELP_H)
  check_include_files("windows.h;libloaderapi.h" HAVE_LIBLOADERAPI_H)
  check_include_files("windows.h;psapi.h" HAVE_PSAPI_H)
endif()

check_include_files("dirent.h" HAVE_DIRENT_H)
check_include_files("dlfcn.h" HAVE_DLFCN_H)
check_include_files("link.h" HAVE_LINK_H)

check_symbol_exists("setenv" "stdlib.h" HAVE_SETENV)

if(HAVE_DBGHELP_H)
  target_link_libraries(fortres PRIVATE Dbghelp)
endif()

if(HAVE_DIRENT_H)
  set_source_files_properties(./dirent.c PROPERTIES HEADER_FILE_ONLY YES)
endif()

if(HAVE_DLFCN_H)
  target_link_libraries(fortres PRIVATE dl)
endif()

if(HAVE_PSAPI_H)
  target_link_libraries(fortres PRIVATE psapi)
endif()

file(TO_NATIVE_PATH "/" PATH_SEP)
# escaping native path
string(REPLACE "\\" "\\\\" PATH_SEP "${PATH_SEP}")
set(PATH_SEP_UNIX "/")
set(PATH_SEP_WINDOWS "\\\\")

if(MSVC)
  set(LIB_PATH_VAR "PATH")
  set(LIB_PATH_SEP ";")
else()
  set(LIB_PATH_VAR "LD_LIBRARY_PATH")
  set(LIB_PATH_SEP ":")
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/fortres_config.h.in ${CMAKE_CURRENT_BINARY_DIR}/fortres_config.h)

cmi_set_directory(fortres)
