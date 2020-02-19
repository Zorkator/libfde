
function(new_test)
  set(OutputFileList "")
  foreach(source IN LISTS ARGN)
    get_filename_component(name "${source}" NAME_WE)
    add_executable(fde.${name} ${source})
    target_link_libraries(fde.${name} PRIVATE fde)
    cmi_set_directory(fde.${name} IDE "tests")

    add_test(NAME fde.${name} COMMAND fde.${name})
  endforeach()

endfunction()

# input files for generator
#generateFiles(
#./test_ref_types.F90_tpp
#)


new_test(
./test_abstract_list.F90
./test_charstacker.F90
#./test_crc.F90
./test_dyncast.F90
./test_fde.F90
./test_file.F90
./test_hash_map.F90
./test_item.F90
#./test_ref.F90
#./test_ref_types.F90
#./test_simulator.F90
#./test_simulator_driver.F90
./test_string.F90
./test_plugin.F90
)
