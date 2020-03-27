# Compiler flags
cmi_Fortran_append(CMAKE_Fortran_FLAGS TRACEBACK)
cmi_fortran_append(CMAKE_Fortran_FLAGS FPP)

cmi_Fortran_append(CMAKE_Fortran_FLAGS_DEBUG TRAPUV)
cmi_Fortran_append(CMAKE_Fortran_FLAGS_DEBUG CHECKALL)

if(MSVC)
else()
  if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
    set_source_files_properties(./crc_impl.F90 APPEND PROPERTIES COMPILE_FLAGS "-fno-range-check")
  elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
    set_source_files_properties(./crc_impl.F90 APPEND PROPERTIES COMPILE_FLAGS "-assume noold_boz")
  endif()
endif()

# fde input files for generator
set(generate_files
  ./basetypes.F90_tpp
  ./containertypes.F90_tpp
  ./hashmap.F90_tpp
  ./hashmap_impl.F90_tpp
  ./item.F90_tpp
  ./list.F90_tpp
  ./ref.F90_tpp
  ./string.F90_tpp
)
set(OutputFileList "")
foreach(inFile IN LISTS generate_files)
  string(REPLACE "_tpp" "" outFile "${inFile}")
  set(inFile "${CMAKE_CURRENT_SOURCE_DIR}/${inFile}")
  set(outFile "${CMAKE_CURRENT_BINARY_DIR}/${outFile}")
  get_filename_component(outDir "${outFile}" DIRECTORY)
  add_custom_command(
    OUTPUT "${outFile}"
    COMMAND "${CMAKE_COMMAND}" -E make_directory "${outDir}"
    COMMAND "${Python_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/../typegen.py" "${inFile}" -o "${outFile}"
    DEPENDS "${inFile}"
  )
  list(APPEND OutputFileList "${outFile}")
endforeach()
add_custom_target(fde_gen DEPENDS ${OutputFileList} SOURCES ${generate_files})
cmi_set_directory(fde_gen)

# path to store Fortran modules
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")

# fde
add_library(fde SHARED
  ./basetypes.F90
  ./containertypes.F90
  ./hashmap.F90
  ./hashmap_impl.F90
  ./item.F90
  ./list.F90
  ./ref.F90
  ./string.F90

  ./basestring.F90
  ./basestring_impl.F90
  ./bisect.F90
  ./charstacker.F90
  ./charstacker_impl.F90
  ./convert.F90
  ./crc.F90
  ./crc_impl.F90
  ./file.F90
  ./fortres_modules.F90
  ./item_impl.F90
  ./list_impl.F90
  ./memoryref.F90
  ./ostream.F90
  ./ref_impl.F90
  ./scope.F90
  ./scope_impl.F90
  ./sort.F90
  ./streamvisitor.F90
  ./string_impl.F90
  ./typeinfo.F90
  ./typeinfo_impl.F90
  ./visitor.F90
)
# generate source files
add_dependencies(fde fde_gen)

target_link_libraries(fde PRIVATE fortres)
target_include_directories(fde PUBLIC ../include)
target_include_directories(fde PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}")

set_property(TARGET fde PROPERTY VERSION ${FDE_VERSION})
set_property(TARGET fde PROPERTY SOVERSION ${FDE_VERSION_SO})

if(MSVC)
  target_compile_definitions(fde PRIVATE BUILT_TYPE=SHARED_LIB)
  set_property(TARGET fde PROPERTY OUTPUT_NAME ${FDE_NAME}.${FDE_VERSION})
else()
  set_property(TARGET fde PROPERTY OUTPUT_NAME ${FDE_NAME})
endif()

if(NOT MSVC)
  set_property(TARGET fde PROPERTY LINKER_LANGUAGE CXX)
endif()

cmi_set_directory(fde)
