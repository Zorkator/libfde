
function(new_test)
  set(OutputFileList "")
  foreach(source IN LISTS ARGN)
    get_filename_component(name "${source}" NAME_WE)
    add_executable(fde.${name} ${source})
    target_link_libraries(fde.${name} PRIVATE fde)
    set_target_properties(fde.${name} PROPERTIES FOLDER tests)
    
    add_test(NAME fde.${name} COMMAND fde.${name})
    
    # Todo: collect all folders where to put runtime libs
    if(WIN32 OR CYGWIN)
      # copy dynamic libraries when rpath is not available
      if(fde_type STREQUAL "SHARED_LIBRARY")
        add_custom_command(
          TARGET fde.${name} POST_BUILD
          COMMAND ${CMAKE_COMMAND} -E copy_if_different
                  "$<TARGET_FILE:fde>"
                  "$<TARGET_FILE_DIR:fde.${name}>/$<TARGET_FILE_NAME:fde>"
        )
      endif()
      if(fortres_type STREQUAL "SHARED_LIBRARY")
        add_custom_command(
          TARGET fde.${name} POST_BUILD
          COMMAND ${CMAKE_COMMAND} -E copy_if_different
                  "$<TARGET_FILE:fortres>"
                  "$<TARGET_FILE_DIR:fde.${name}>/$<TARGET_FILE_NAME:fortres>"
        )
      endif()
    endif()
  endforeach()

endfunction()

# input files for generator
#generateFiles(
#./test_ref_types.F90_tpp
#)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

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
