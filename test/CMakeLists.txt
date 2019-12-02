
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