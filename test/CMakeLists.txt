
function(new_test)
  set(OutputFileList "")
  foreach(source IN LISTS ARGN)
    get_filename_component(name "${source}" NAME_WE)
    add_executable(${name} ${source})
    target_link_libraries(${name} PRIVATE fde)
    add_test(NAME test.${name} COMMAND $<TARGET_FILE:${name}>)
    set_target_properties (${name} PROPERTIES FOLDER tests)
  endforeach()
  if(WIN32)
    # copy dynamic libraries when rpath is not available
    get_target_property(fde_type fde TYPE)
    if(fde_type STREQUAL "SHARED_LIBRARY")
      add_custom_command(
        TARGET ${name} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
                "$<TARGET_FILE:fde>"
                "$<TARGET_FILE_DIR:${name}>/$<TARGET_FILE_NAME:fde>"
      )
    endif()
    get_target_property(fortres_type fortres TYPE)
    if(fortres_type STREQUAL "SHARED_LIBRARY")
      add_custom_command(
        TARGET ${name} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
                "$<TARGET_FILE:fortres>"
                "$<TARGET_FILE_DIR:${name}>/$<TARGET_FILE_NAME:fortres>"
      )
    endif()
  endif()
endfunction()

# input files for generator
#generateFiles(
#./test_ref_types.f90_tpp
#)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

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