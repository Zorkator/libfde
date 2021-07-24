# Compiler flags
cmi_Fortran_append(CMAKE_Fortran_FLAGS STACKARRAYLIMIT65)
cmi_Fortran_append(CMAKE_Fortran_FLAGS TRACEBACK)
cmi_fortran_append(CMAKE_Fortran_FLAGS EXTENDEDLINESF90)

cmi_Fortran_append(CMAKE_Fortran_FLAGS_DEBUG TRAPUV)
cmi_Fortran_append(CMAKE_Fortran_FLAGS_DEBUG CHECKALL)

# fde input files for generator
set(generate_files
  ./test_ref_types.F90_tpp
)
set(OutputFileList "")
# Multiproject generators like VS will always generate files on initial build of a config,
# despite of correct rules and up to date timestamps from previous builds of other configs.
# This might cause unnecessary recompilation of other projects depending on this.
# Fix: Insert an additional step that will only overwrite generated files,
# if the timestamp is outdated.
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/generate_file.cmake"
"if($\{INPUT_FILE} IS_NEWER_THAN \${OUTPUT_FILE})
execute_process(COMMAND \"${Python_EXECUTABLE}\" \"${FDE_GENERATOR_SCRIPT}\" \"\${INPUT_FILE}\" -o \"\${OUTPUT_FILE}\")
endif()")

foreach(inFile IN LISTS generate_files)
  string(REPLACE "_tpp" "" outFile "${inFile}")
  set(inFile "${CMAKE_CURRENT_SOURCE_DIR}/${inFile}")
  set(outFile "${CMAKE_CURRENT_BINARY_DIR}/${outFile}")
  get_filename_component(outDir "${outFile}" DIRECTORY)
  add_custom_command(
    OUTPUT "${outFile}"
    COMMAND "${CMAKE_COMMAND}" -E make_directory "${outDir}"
    COMMAND "${CMAKE_COMMAND}"
            -D INPUT_FILE="${inFile}"
            -D OUTPUT_FILE="${outFile}"
            -P "${CMAKE_CURRENT_BINARY_DIR}/generate_file.cmake"
    DEPENDS "${inFile}" "${FDE_GENERATOR_SCRIPT}"
  )
  list(APPEND OutputFileList "${outFile}")
endforeach()
add_custom_target(fde_test_gen DEPENDS ${OutputFileList} SOURCES ${generate_files})
cmi_set_directory(fde_test_gen)

set(tests
  ./test_abstract_list.F90
  ./test_charstacker.F90
  ./test_crc.F90
  ./test_dyncast.F90
  ./test_fde.F90
  ./test_file.F90
  ./test_hash_map.F90
  ./test_item.F90
  ./test_ref.F90
  ./test_simulator.F90
  #./test_simulator_driver.F90
  ./test_sorting.F90
  ./test_string.F90
  ./test_plugin.F90
)

foreach(source IN LISTS tests)
  get_filename_component(name "${source}" NAME_WE)
  add_executable(fde.${name} ${source})
  add_dependencies(fde.${name} fde_test_gen)
  target_link_libraries(fde.${name} PRIVATE fde)
  cmi_set_directory(fde.${name} IDE "tests")
  add_test(NAME fde.${name} COMMAND fde.${name})
endforeach()

target_sources(fde.test_ref PRIVATE ./test_ref_types.F90)
