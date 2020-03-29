cmi_fortran_append(CMAKE_Fortran_FLAGS FPP)

# fde input files for generator
set(generate_files
  ./test_ref_types.F90_tpp
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
