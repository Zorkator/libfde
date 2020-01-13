cmake_minimum_required(VERSION 3.2)
project(fde Fortran)

# Set name and version.
set(FDE_NAME "fde")
set(FDE_VERSION_MAJOR 2)
set(FDE_VERSION_MINOR 7)
set(FDE_VERSION_PATCH 3)
set(FDE_VERSION       "${FDE_VERSION_MAJOR}.${FDE_VERSION_MINOR}.${FDE_VERSION_PATCH}")
# cmake test module
enable_testing()

include(cmiLoader.cmake)
set(cmi_tag 3b4f2c3a785898fb3de24e15f884538a10336cc3)
cmi_add_loader("https://gitlab.com/nordfox/cmakeit/-/archive/${cmi_tag}/cmakeit-${cmi_tag}.tar.gz")

include(cmiPython)
cmi_python_check_packages(docopt)

# Compiler flags
include(cmiCompilerFlags)
cmi_Fortran_append(CMAKE_Fortran_FLAGS FPP)
if(WIN32)
else()
  if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
    set_source_files_properties(src/crc_impl.F90 APPEND PROPERTIES COMPILE_FLAGS "-fno-range-check")
  elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
    set_source_files_properties(src/crc_impl.F90 APPEND PROPERTIES COMPILE_FLAGS "-assume noold_boz")
  endif()
endif()


# Custom rules for source code generator
function(generateFiles)
  set(OutputFileList "")
  foreach(inFile IN LISTS ARGN)
    string(REPLACE "_tpp" "" outFile "${inFile}")
    set(inFile "${CMAKE_CURRENT_SOURCE_DIR}/${inFile}")
    set(outFile "${CMAKE_CURRENT_BINARY_DIR}/${outFile}")
    get_filename_component(outDir "${outFile}" DIRECTORY)
    add_custom_command(
      OUTPUT "${outFile}"
      COMMAND "${CMAKE_COMMAND}" -E make_directory "${outDir}"
      COMMAND "${Python_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/typegen.py" "${inFile}" -o "${outFile}"
      DEPENDS "${inFile}"
    )
    list(APPEND OutputFileList "${outFile}")
  endforeach()
  add_custom_target(fde_gen DEPENDS ${OutputFileList} SOURCES ${ARGN})
endfunction()



# include path for this and all subprojects
include_directories(include)

# fortres
add_subdirectory(libfortres)

# path to store Fortran modules
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include")


# fde input files for generator
generateFiles(
  src/basetypes.F90_tpp
  src/containertypes.F90_tpp
  src/hashmap.F90_tpp
  src/hashmap_impl.F90_tpp
  src/item.F90_tpp
  src/list.F90_tpp
  src/ref.F90_tpp
  src/string.F90_tpp
)

# fde
add_library(fde SHARED
  src/basetypes.F90
  src/containertypes.F90
  src/hashmap.F90
  src/hashmap_impl.F90
  src/item.F90
  src/list.F90
  src/ref.F90
  src/string.F90

  src/basestring.F90
  src/basestring_impl.F90
  src/bisect.F90
  src/charstacker.F90
  src/charstacker_impl.F90
  src/convert.F90
  src/crc.F90
  src/crc_impl.F90
  src/file.F90
  src/fortres_modules.F90
  src/item_impl.F90
  src/list_impl.F90
  src/memoryref.F90
  src/ostream.F90
  src/ref_impl.F90
  src/scope.F90
  src/scope_impl.F90
  src/sort.F90
  src/streamvisitor.F90
  src/string_impl.F90
  src/typeinfo.F90
  src/typeinfo_impl.F90
  src/visitor.F90
)
# generate source files
add_dependencies(fde fde_gen)

target_link_libraries(fde PRIVATE fortres)
target_include_directories(fde PUBLIC include)
target_include_directories(fde PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}")

if(WIN32)
  target_compile_definitions(fde PRIVATE BUILT_TYPE=SHARED_LIB)
  set_property(TARGET fde PROPERTY OUTPUT_NAME ${FDE_NAME}.${FDE_VERSION})
else()
  set_property(TARGET fde PROPERTY OUTPUT_NAME ${FDE_NAME})
endif()

# Shared/static library options
get_target_property(fde_type fde TYPE)
get_target_property(fortres_type fortres TYPE)
if(fde_type STREQUAL "SHARED_LIBRARY")
  set_property(TARGET fortres PROPERTY POSITION_INDEPENDENT_CODE ON)
  if(NOT WIN32)
    include(CheckLanguage)
    check_language(CXX)
    if(CMAKE_CXX_COMPILER)
      enable_language(CXX)
    else ()
      message(WARNING "No CXX support. Linking might fail.")
    endif ()
    set_property(TARGET fde PROPERTY LINKER_LANGUAGE CXX)
  endif()
endif()

# tests
enable_testing()
add_subdirectory(test)

# IDE settings
set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set_target_properties(fde PROPERTIES FOLDER libraries)
set_target_properties(fde_gen PROPERTIES FOLDER libraries)
set_target_properties(fortres PROPERTIES FOLDER libraries)