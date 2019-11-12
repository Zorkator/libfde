cmake_minimum_required(VERSION 2.8.12)
project(fde Fortran)

if(WIN32)

else()
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")
  if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fbacktrace")
    set_source_files_properties(src/crc_impl.f90 APPEND PROPERTIES COMPILE_FLAGS "-ffree-line-length-none")
    set_source_files_properties(src/crc_impl.f90 APPEND PROPERTIES COMPILE_FLAGS "-fno-range-check")
  elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -traceback")
    set_source_files_properties(src/crc_impl.f90 APPEND PROPERTIES COMPILE_FLAGS "-assume noold_boz")
  endif()
endif()

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
      COMMAND python "${CMAKE_CURRENT_SOURCE_DIR}/typegen.py"
      "${inFile}"
      -o "${outFile}"
      DEPENDS "${inFile}"
    )
    list(APPEND OutputFileList "${outFile}")
  endforeach()
  add_custom_target(
    fde_gen
    DEPENDS ${OutputFileList}
  )
endfunction()

generateFiles(
  src/basetypes.f90_tpp
  src/containertypes.f90_tpp
  src/hashmap.f90_tpp
  src/hashmap_impl.f90_tpp
  src/item.f90_tpp
  src/list.f90_tpp
  src/ref.f90_tpp
  src/string.f90_tpp
)

# External Libs
include_directories(include)
add_subdirectory(libfortres)
add_library(fde SHARED
  src/basetypes.f90
  src/containertypes.f90
  src/hashmap.f90
  src/hashmap_impl.f90
  src/item.f90
  src/list.f90
  src/ref.f90
  src/string.f90

  src/basestring.f90
  src/basestring_impl.f90
  src/bisect.f90
  src/charstacker.f90
  src/charstacker_impl.f90
  src/convert.f90
  src/crc.f90
  src/crc_impl.f90
  src/file.f90
  src/fortres_modules.f90
  src/item_impl.f90
  src/list_impl.f90
  src/memoryref.f90
  src/ostream.f90
  src/ref_impl.f90
  src/scope.f90
  src/scope_impl.f90
  src/sort.f90
  src/streamvisitor.f90
  src/string_impl.f90
  src/typeinfo.f90
  src/typeinfo_impl.f90
  src/visitor.f90
)

add_dependencies(fde fde_gen)
target_link_libraries(fde PRIVATE fortres)
target_include_directories(fde PUBLIC include)

target_compile_definitions(fde PRIVATE BUILT_TYPE=SHARED_LIB)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)
set_property(TARGET fortres PROPERTY POSITION_INDEPENDENT_CODE ON)
