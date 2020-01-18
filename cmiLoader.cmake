cmake_minimum_required(VERSION 3.8)

set(cmi_TAG "95a141e3d245d22de463bab7a516c0ff456b29f9")

get_property(cmi_LOADER_FILE GLOBAL PROPERTY cmi_LOADER_FILE)
if(NOT cmi_LOADER_FILE)
  set_property(GLOBAL PROPERTY cmi_LOADER_FILE "${CMAKE_CURRENT_LIST_FILE}")
  if(DEFINED cmi_DOWNLOAD_TAG AND NOT ("${cmi_DOWNLOAD_TAG}" STREQUAL "${cmi_TAG}"))
    message(STATUS "Update ${CMAKE_CURRENT_LIST_FILE} to ${cmi_DOWNLOAD_TAG}")
    set(cmi_LOADER_TMP "${CMAKE_BINARY_DIR}/cmiLoader.cmake")
    file(
      DOWNLOAD "https://gitlab.com/nordfox/cmakeit/raw/${cmi_DOWNLOAD_TAG}/cmiLoader.cmake"
      "${cmi_LOADER_TMP}.in"
      SHOW_PROGRESS
    )
    configure_file("${cmi_LOADER_TMP}.in" "${cmi_LOADER_TMP}" @ONLY NEWLINE_STYLE UNIX)
    execute_process(
      COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${cmi_LOADER_TMP}" "${CMAKE_CURRENT_LIST_FILE}"
    )
    include("${cmi_LOADER_TMP}")
    file(REMOVE "${cmi_LOADER_TMP}.in" "${cmi_LOADER_TMP}")
    return()
  endif()
endif()

get_property(cmi_LOADER_INCLUDED GLOBAL PROPERTY cmi_LOADER_INCLUDED)
if(cmi_LOADER_INCLUDED)
  return()
endif()
set_property(GLOBAL PROPERTY cmi_LOADER_INCLUDED "TRUE")

set(cmi_EXTERNALS_DIR "${CMAKE_SOURCE_DIR}/_externals/" CACHE PATH "")

function(cmi_add_archive_url PROJECT_NAME PROJECT_URL)
  get_property(${PROJECT_NAME}_POPULATED GLOBAL PROPERTY ${PROJECT_NAME}_POPULATED)
  if(${PROJECT_NAME}_POPULATED)
    return()
  endif()
  set_property(GLOBAL PROPERTY ${PROJECT_NAME}_POPULATED "TRUE")
  
  if(DEFINED ${PROJECT_NAME}_URL_CURRENT AND NOT ("${PROJECT_URL}" STREQUAL "${${PROJECT_NAME}_URL_CURRENT}"))
    unset(${PROJECT_NAME}_DIR CACHE)
  endif()
  get_filename_component(${PROJECT_NAME}_ARCHIVE "${PROJECT_URL}" NAME)
  set(${PROJECT_NAME}_ARCHIVE_PATH "${cmi_EXTERNALS_DIR}/${${PROJECT_NAME}_ARCHIVE}")
  get_filename_component(${PROJECT_NAME}_ARCHIVE_TAG "${${PROJECT_NAME}_ARCHIVE_PATH}" NAME_WE)
  set(${PROJECT_NAME}_DIR "${cmi_EXTERNALS_DIR}/${${PROJECT_NAME}_ARCHIVE_TAG}" CACHE PATH "")
  
  if(NOT EXISTS "${${PROJECT_NAME}_DIR}")
    if(NOT EXISTS "${${PROJECT_NAME}_ARCHIVE_PATH}")
      message(STATUS "Downloading ${PROJECT_NAME} ${PROJECT_URL}")
      file(DOWNLOAD "${PROJECT_URL}" "${${PROJECT_NAME}_ARCHIVE_PATH}" SHOW_PROGRESS)
    endif()
    message(STATUS "Extracting ${PROJECT_NAME} ${${PROJECT_NAME}_ARCHIVE_PATH}")
    execute_process(
      COMMAND "${CMAKE_COMMAND}" -E tar -xf "${${PROJECT_NAME}_ARCHIVE_PATH}"
      WORKING_DIRECTORY "${cmi_EXTERNALS_DIR}"
    )
  endif()
  
  set(${PROJECT_NAME}_URL_CURRENT "${PROJECT_URL}" CACHE INTERNAL "")
  
  if(EXISTS "${${PROJECT_NAME}_DIR}/cmake")
    set(CMAKE_MODULE_PATH "${${PROJECT_NAME}_DIR}/cmake" "${CMAKE_MODULE_PATH}" CACHE INTERNAL "")
  endif()
endfunction()
