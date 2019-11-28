
function(new_test)
  set(OutputFileList "")
  foreach(source IN LISTS ARGN)
    get_filename_component(name "${source}" NAME_WE)
    add_executable(${name} ${source})
    target_link_libraries(${name} PRIVATE fde)
    add_test(NAME test.${name} COMMAND $<TARGET_FILE:${name}>)
    list(APPEND OutputFileList "${outFile}")
  endforeach()
  #add_custom_command(
  #  TARGET test_fde POST_BUILD
  #  COMMAND "${CMAKE_COMMAND}" -E copy "$<TARGET_FILE:fde>" "$<TARGET_FILE_DIR:test_fde>/$<TARGET_FILE_NAME:fde>"
  #)
  #add_custom_command(
  #  TARGET test_fde POST_BUILD
  #  COMMAND "${CMAKE_COMMAND}" -E copy "$<TARGET_FILE:fortres>" "$<TARGET_FILE_DIR:test_fde>/$<TARGET_FILE_NAME:fortres>"
  #)
  #add_custom_target(fde_gen DEPENDS ${OutputFileList} SOURCES ${ARGN})
endfunction()

# input files for generator
#generateFiles(
#./test_ref_types.f90_tpp
#)

new_test(
./test_abstract_list.f90
./test_charstacker.f90
#./test_crc.f90
./test_dyncast.f90
./test_fde.f90
./test_file.f90
./test_hash_map.f90
./test_item.f90
#./test_ref.f90
#./test_ref_types.f90
#./test_simulator.f90
#./test_simulator_driver.f90
./test_string.f90
)