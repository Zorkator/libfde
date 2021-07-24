# Compiler flags
cmi_Fortran_append(CMAKE_Fortran_FLAGS STACKARRAYLIMIT65)
cmi_fortran_append(CMAKE_Fortran_FLAGS EXTENDEDLINESF90)

cmi_Fortran_append(CMAKE_Fortran_FLAGS_DEBUG TRAPUV)

if(NOT "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel")
  # Intel C++ and Traceback is buggy and results in infinite recursion loop
  cmi_Fortran_append(CMAKE_Fortran_FLAGS TRACEBACK)
endif()

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-range-check")
else()
  # Many most likely false positive errors with recent gfortran compilers
  cmi_Fortran_append(CMAKE_Fortran_FLAGS_DEBUG CHECKBASIC)
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


# Multiproject generators like VS will always generate files on initial build of a config,
# despite of correct rules and up to date timestamps from previous builds of other configs.
# This might cause unnecessary recompilation of other projects depending on this.
# Fix: Insert an additional step that will only overwrite generated files,
# if the actual content has changed.
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/copy_file.cmake" "configure_file(\${INPUT_FILE} \${OUTPUT_FILE} COPYONLY)")

foreach(inFile IN LISTS generate_files)
  string(REPLACE "_tpp" "" outFile "${inFile}")
  set(inFile "${CMAKE_CURRENT_SOURCE_DIR}/${inFile}")
  set(outFile "${CMAKE_CURRENT_BINARY_DIR}/${outFile}")
  get_filename_component(outDir "${outFile}" DIRECTORY)
  add_custom_command(
    OUTPUT "${outFile}"
    COMMAND "${CMAKE_COMMAND}" -E make_directory "${outDir}"
    COMMAND "${Python_EXECUTABLE}" "${FDE_GENERATOR_SCRIPT}" "${inFile}" -o "${outFile}.tmp"
    COMMAND "${CMAKE_COMMAND}"
            -D INPUT_FILE="${outFile}.tmp"
            -D OUTPUT_FILE="${outFile}"
            -P "${CMAKE_CURRENT_BINARY_DIR}/copy_file.cmake"
    DEPENDS "${inFile}" "${FDE_GENERATOR_SCRIPT}"
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
