# BSD 2-Clause License
# Copyright (c) 2019, Volker Jacht
# All rights reserved.
# See https://gitlab.com/nordfox/cmakeit

cmake_minimum_required(VERSION 3.8)

set(CMI_TAG "5346dc2806e39ff86d53a28406c49d7693505dac")

get_property(CMI_LOADER_FILE GLOBAL PROPERTY CMI_LOADER_FILE)
# First include
if(NOT CMI_LOADER_FILE)
  set_property(GLOBAL PROPERTY CMI_LOADER_FILE "${CMAKE_CURRENT_LIST_FILE}")
  # Check for updates
  if(DEFINED CMI_DOWNLOAD_TAG AND NOT ("${CMI_DOWNLOAD_TAG}" STREQUAL "${CMI_TAG}"))
    message(STATUS "Update ${CMAKE_CURRENT_LIST_FILE} to ${CMI_DOWNLOAD_TAG}")
    get_filename_component(CMI_FILENAME_ ${CMAKE_CURRENT_LIST_FILE} NAME)
    set(CMI_LOADER_TMP_ "${CMAKE_CURRENT_BINARY_DIR}/${CMI_FILENAME_}")
    file(
      DOWNLOAD "https://gitlab.com/nordfox/cmakeit/raw/${CMI_DOWNLOAD_TAG}/cmi.cmake"
      "${CMI_LOADER_TMP_}.in"
      SHOW_PROGRESS
    )
    file(SIZE "${CMI_LOADER_TMP_}.in" CMI_LOADER_FILESIZE_)
    if(NOT CMI_LOADER_FILESIZE_ STREQUAL 0)
      configure_file("${CMI_LOADER_TMP_}.in" "${CMI_LOADER_TMP_}" @ONLY NEWLINE_STYLE UNIX)
      execute_process(
        COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${CMI_LOADER_TMP_}" "${CMAKE_CURRENT_LIST_FILE}"
      )
      include("${CMI_LOADER_TMP_}")
      file(REMOVE "${CMI_LOADER_TMP_}.in" "${CMI_LOADER_TMP_}")

      unset(CMI_FILENAME_)
      unset(CMI_LOADER_TMP_)
      return()
    else()
      message(WARNING "Updating CMI failed! Using existing version.")
    endif()

  endif()
endif()

# Check if CMI is loaded at local scope
if(CMI_LOADED)
  return()
endif()

# Always use the first included CMI file
get_property(CMI_LOADER_FILE GLOBAL PROPERTY CMI_LOADER_FILE)
if(NOT (CMI_LOADER_FILE STREQUAL CMAKE_CURRENT_LIST_FILE))
  include("${CMI_LOADER_FILE}")
  return()
endif()
unset(CMI_LOADER_FILE)

set(CMI_LOADED TRUE)

# Consider root project as populated and included
string(TOUPPER "${CMAKE_PROJECT_NAME}" PROJECT_NAME_UPPER_)
set_property(GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_INCLUDED "TRUE")
unset(PROJECT_NAME_UPPER_)

set(CMI_EXTERNALS_DIR "${CMAKE_SOURCE_DIR}/_externals" CACHE PATH "")
mark_as_advanced(CMI_EXTERNALS_DIR)
set(CMI_ENABLE_PREFIX "CMI_ENABLE_")

######################################
# FETCHING
######################################

# PROPERTY, POPULATED => ready to include / up to date
# PROPERTY, INCLUDED => already inclued somewhere
# CACHE, EXTERNAL_TYPE
# CACHE, EXTERNAL_URL
# CACHE, ENABLE


function(cmi_add_subdirectory PROJECT_NAME_)
  string(TOUPPER "${PROJECT_NAME_}" PROJECT_NAME_UPPER_)
  string(TOLOWER "${PROJECT_NAME_}" PROJECT_NAME_LOWER_)

  get_property(${PROJECT_NAME_UPPER_}_INCLUDED_ GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_INCLUDED)
  if(${PROJECT_NAME_UPPER_}_INCLUDED_)
    # project already been included
    return()
  endif()

  cmake_parse_arguments(CMI_ADD_SUBDIRECTORY "NO_CMAKE" "OPTIONAL" "" ${ARGN})
  list(FIND CMI_ADD_SUBDIRECTORY_KEYWORDS_MISSING_VALUES "OPTIONAL" RESULT_)
  if(NOT RESULT_ STREQUAL -1)
    set(CMI_ADD_SUBDIRECTORY_OPTIONAL OFF)
  endif()

  if(DEFINED CMI_ADD_SUBDIRECTORY_OPTIONAL)
    # optional => add to options
    if(NOT DEFINED ${CMI_ENABLE_PREFIX}${PROJECT_NAME_UPPER_})
      option(${CMI_ENABLE_PREFIX}${PROJECT_NAME_UPPER_} "" "${CMI_ADD_SUBDIRECTORY_OPTIONAL}")
    endif()
  else()
    # not optional => force enabled
    set(${CMI_ENABLE_PREFIX}${PROJECT_NAME_UPPER_} ON CACHE BOOL "" FORCE)
  endif()

  if(${CMI_ENABLE_PREFIX}${PROJECT_NAME_UPPER_})
    get_property(${PROJECT_NAME_UPPER_}_POPULATED_ GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_POPULATED)
    if(NOT ${PROJECT_NAME_UPPER_}_POPULATED_)
      if(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE STREQUAL "archive")
        cmi_add_archive_(${PROJECT_NAME_UPPER_} ${CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL})
      elseif(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE STREQUAL "git")
        cmi_add_git_(${PROJECT_NAME_UPPER_} ${CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL} ${CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_REV})
      elseif(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE STREQUAL "svn")
        cmi_add_svn_(${PROJECT_NAME_UPPER_} ${CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL} ${CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_REV})
      endif()
    endif()
    if(NOT DEFINED CMI_ADD_SUBDIRECTORY_OPTIONAL)
      get_property(${PROJECT_NAME_UPPER_}_POPULATED_ GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_POPULATED)
      if(NOT ${PROJECT_NAME_UPPER_}_POPULATED_)
        message(WARNING "Can not proceed as ${PROJECT_NAME_UPPER_} is missing")
      endif()
    endif()

    if(EXISTS ${${PROJECT_NAME_UPPER_}_DIR})
      if(NOT CMI_ADD_SUBDIRECTORY_NO_CMAKE)
        set_property(GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_INCLUDED "TRUE")
        add_subdirectory("${${PROJECT_NAME_UPPER_}_DIR}" "${CMAKE_BINARY_DIR}/externals/${PROJECT_NAME_LOWER_}")
      endif()
    else()
      message(WARNING "Can not include ${${PROJECT_NAME_UPPER_}_DIR}")
    endif()
  endif()
endfunction()


function(cmi_add_archive PROJECT_NAME_ PROJECT_URL_)
  string(TOUPPER "${PROJECT_NAME_}" PROJECT_NAME_UPPER_)
  set(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE "archive" CACHE STRING "")
  set(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL "${PROJECT_URL_}" CACHE STRING "")
  mark_as_advanced(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE)
  mark_as_advanced(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL)
endfunction()

function(cmi_add_git PROJECT_NAME_ PROJECT_URL_ PROJECT_REV_)
  string(TOUPPER "${PROJECT_NAME_}" PROJECT_NAME_UPPER_)
  set(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE "git" CACHE STRING "")
  set(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL "${PROJECT_URL_}" CACHE STRING "")
  set(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_REV "${PROJECT_REV_}" CACHE STRING "")
  mark_as_advanced(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE)
  mark_as_advanced(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL)
  mark_as_advanced(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_REV)
endfunction()

function(cmi_add_svn PROJECT_NAME_ PROJECT_URL_ PROJECT_REV_)
  string(TOUPPER "${PROJECT_NAME_}" PROJECT_NAME_UPPER_)
  set(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE "svn" CACHE STRING "")
  set(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL "${PROJECT_URL_}" CACHE STRING "")
  set(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_REV "${PROJECT_REV_}" CACHE STRING "")
  mark_as_advanced(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE)
  mark_as_advanced(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL)
  mark_as_advanced(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_REV)
endfunction()

function(cmi_add_archive_ PROJECT_NAME_ PROJECT_URL_)
  string(TOUPPER "${PROJECT_NAME_}" PROJECT_NAME_UPPER_)
  get_property(${PROJECT_NAME_UPPER_}_POPULATED GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_POPULATED)
  if(${PROJECT_NAME_UPPER_}_POPULATED)
    return()
  endif()
  set_property(GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_POPULATED "TRUE")

  if(DEFINED ${PROJECT_NAME_UPPER_}_URL_CURRENT AND NOT ("${PROJECT_URL_}" STREQUAL "${${PROJECT_NAME_UPPER_}_URL_CURRENT}"))
    unset(${PROJECT_NAME_UPPER_}_DIR CACHE)
  endif()
  get_filename_component(${PROJECT_NAME_UPPER_}_ARCHIVE "${PROJECT_URL_}" NAME)
  set(${PROJECT_NAME_UPPER_}_ARCHIVE_PATH "${CMI_EXTERNALS_DIR}/${${PROJECT_NAME_UPPER_}_ARCHIVE}")
  string(REPLACE ".tar.gz" "" ${PROJECT_NAME_UPPER_}_ARCHIVE_TAG "${${PROJECT_NAME_UPPER_}_ARCHIVE}")
  set(${PROJECT_NAME_UPPER_}_DIR "${CMI_EXTERNALS_DIR}/${${PROJECT_NAME_UPPER_}_ARCHIVE_TAG}" CACHE PATH "")

  if(NOT EXISTS "${${PROJECT_NAME_UPPER_}_DIR}")
    if(NOT EXISTS "${${PROJECT_NAME_UPPER_}_ARCHIVE_PATH}")
      message(STATUS "Downloading ${PROJECT_NAME_UPPER_} ${PROJECT_URL_}")
      file(DOWNLOAD "${PROJECT_URL_}" "${${PROJECT_NAME_UPPER_}_ARCHIVE_PATH}" SHOW_PROGRESS)
      file(SIZE "${${PROJECT_NAME_UPPER_}_ARCHIVE_PATH}" ARCHIVE_FILESIZE_)
      if(ARCHIVE_FILESIZE_ STREQUAL 0)
        file(REMOVE "${${PROJECT_NAME_UPPER_}_ARCHIVE_PATH}")
        message(WARNING "Can not download ${PROJECT_URL_}")
      endif()
    endif()
    message(STATUS "Extracting ${PROJECT_NAME_UPPER_} ${${PROJECT_NAME_UPPER_}_ARCHIVE_PATH} to ${${PROJECT_NAME_UPPER_}_DIR}")
    execute_process(
      COMMAND "${CMAKE_COMMAND}" -E tar -xzf "${${PROJECT_NAME_UPPER_}_ARCHIVE_PATH}"
      WORKING_DIRECTORY "${CMI_EXTERNALS_DIR}"
    )
  endif()

  set(${PROJECT_NAME_UPPER_}_URL_CURRENT "${PROJECT_URL_}" CACHE INTERNAL "")

endfunction()


macro(cmi_git_clone_)
  foreach(try RANGE 2)
    message(STATUS "${PROJECT_NAME_UPPER_}: Clone ${PROJECT_URL_}")
    execute_process(
      COMMAND ${GIT_EXECUTABLE} clone ${PROJECT_URL_} "${${PROJECT_NAME_UPPER_}_DIR}"
      RESULT_VARIABLE ${PROJECT_NAME_UPPER_}_RESULT
    )
    if(${PROJECT_NAME_UPPER_}_RESULT STREQUAL 0)
      break()
    endif()
    message(STATUS "${PROJECT_NAME_UPPER_}: Git clone failed.")
  endforeach()
endmacro()

macro(cmi_git_fetch_)
  foreach(try RANGE 2)
    message(STATUS "${PROJECT_NAME_UPPER_}: Fetch ${PROJECT_URL_}")
    execute_process(
      COMMAND ${GIT_EXECUTABLE} remote set-url origin ${PROJECT_URL_}
      COMMAND ${GIT_EXECUTABLE} fetch
      WORKING_DIRECTORY "${${PROJECT_NAME_UPPER_}_DIR}"
      RESULT_VARIABLE ${PROJECT_NAME_UPPER_}_RESULT
    )
    if(${PROJECT_NAME_UPPER_}_RESULT STREQUAL 0)
      break()
    endif()
    message(STATUS "${PROJECT_NAME_UPPER_}: Git fetch failed.")
  endforeach()
endmacro()

macro(cmi_git_checkout_)
  foreach(try RANGE 1)
    if(NOT (try STREQUAL 0))
      cmi_git_fetch_()
    endif()
    message(STATUS "${PROJECT_NAME_UPPER_}: Checkout ${PROJECT_REV_}")
    execute_process(
      COMMAND ${GIT_EXECUTABLE} -c advice.detachedHead=false checkout ${PROJECT_REV_}
      WORKING_DIRECTORY "${${PROJECT_NAME_UPPER_}_DIR}"
      RESULT_VARIABLE ${PROJECT_NAME_UPPER_}_RESULT
    )
    if(${PROJECT_NAME_UPPER_}_RESULT STREQUAL 0)
      break()
    endif()
    message(STATUS "${PROJECT_NAME_UPPER_}: Git checkout failed.")
  endforeach()
endmacro()

macro(cmi_svn_clone_)
  foreach(try RANGE 2)
    message(STATUS "${PROJECT_NAME_UPPER_}: Clone ${PROJECT_URL_}")
    execute_process(
      COMMAND "${Subversion_SVN_EXECUTABLE}" "checkout" "${PROJECT_URL_}@${PROJECT_REV_}" "${${PROJECT_NAME_UPPER_}_DIR}"
      RESULT_VARIABLE ${PROJECT_NAME_UPPER_}_RESULT
    )
    if(${PROJECT_NAME_UPPER_}_RESULT STREQUAL 0)
      break()
    endif()
    message(STATUS "${PROJECT_NAME_UPPER_}: SVN clone failed.")
  endforeach()
endmacro()

function(cmi_add_git_ PROJECT_NAME_ PROJECT_URL_ PROJECT_REV_)
  string(TOUPPER "${PROJECT_NAME_}" PROJECT_NAME_UPPER_)
  string(TOLOWER "${PROJECT_NAME_}" PROJECT_NAME_LOWER_)
  get_property(${PROJECT_NAME_UPPER_}_POPULATED GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_POPULATED)
  if(${PROJECT_NAME_UPPER_}_POPULATED)
    return()
  endif()
  set_property(GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_POPULATED "TRUE")

  find_package(Git REQUIRED)
  set(${PROJECT_NAME_UPPER_}_DIR "${CMI_EXTERNALS_DIR}/${PROJECT_NAME_LOWER_}" CACHE PATH "")

  # Clone repo
  if(NOT EXISTS "${${PROJECT_NAME_UPPER_}_DIR}")
    cmi_git_clone_()
    cmi_git_checkout_()
  endif()

  # Update repo
  if(CMI_UPDATE OR (DEFINED ${PROJECT_NAME_UPPER_}_REPO_CURRENT AND NOT ("${${PROJECT_NAME_UPPER_}_REPO_CURRENT}" STREQUAL "${PROJECT_URL_}")))
    cmi_git_fetch_()
  endif()

  # Update repo tag
  if(CMI_UPDATE OR (DEFINED ${PROJECT_NAME_UPPER_}_REV_CURRENT AND NOT ("${${PROJECT_NAME_UPPER_}_REV_CURRENT}" STREQUAL "${PROJECT_REV_}")))
    cmi_git_checkout_()
  endif()

  set(${PROJECT_NAME_UPPER_}_REPO_CURRENT "${PROJECT_URL_}" CACHE INTERNAL "")
  set(${PROJECT_NAME_UPPER_}_REV_CURRENT "${PROJECT_REV_}" CACHE INTERNAL "")

endfunction()

function(cmi_add_svn_ PROJECT_NAME_ PROJECT_URL_ PROJECT_REV_)
  string(TOUPPER "${PROJECT_NAME_}" PROJECT_NAME_UPPER_)
  string(TOLOWER "${PROJECT_NAME_}" PROJECT_NAME_LOWER_)
  get_property(${PROJECT_NAME_UPPER_}_POPULATED GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_POPULATED)
  if(${PROJECT_NAME_UPPER_}_POPULATED)
    return()
  endif()
  set_property(GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_POPULATED "TRUE")

  find_package(Subversion REQUIRED)
  set(${PROJECT_NAME_UPPER_}_DIR "${CMI_EXTERNALS_DIR}/${PROJECT_NAME_LOWER_}" CACHE PATH "")

  # clone repo
  if(NOT EXISTS "${${PROJECT_NAME_UPPER_}_DIR}")
    cmi_svn_clone_()
  endif()

  # update repository
  if(CMI_UPDATE OR
    DEFINED ${PROJECT_NAME_UPPER_}_REPO_CURRENT AND DEFINED ${PROJECT_NAME_UPPER_}_REV_CURRENT AND
    (NOT ("${PROJECT_URL_}" STREQUAL "${${PROJECT_NAME_UPPER_}_REPO_CURRENT}") OR
    NOT ("${PROJECT_REV_}" STREQUAL "${${PROJECT_NAME_UPPER_}_REV_CURRENT}"))
  )
    message(STATUS "Switch ${PROJECT_NAME_UPPER_}")
    execute_process(COMMAND "${Subversion_SVN_EXECUTABLE}" "switch" "${PROJECT_URL_}@${PROJECT_REV_}" "${${PROJECT_NAME_UPPER_}_DIR}")
  endif()

  set(${PROJECT_NAME_UPPER_}_REPO_CURRENT "${PROJECT_URL_}" CACHE INTERNAL "")
  set(${PROJECT_NAME_UPPER_}_REV_CURRENT "${PROJECT_REV_}" CACHE INTERNAL "")

endfunction()


######################################
# BUILD SETTINGS
######################################

# Set default path variables
macro(cmi_load_build_environment)
  cmi_set_build_environment(${ARGV})
endmacro()
macro(cmi_set_build_environment)
  set(CMI_PRESET_FOLDER "_targets" CACHE PATH "")
  set(CMI_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/package/$<CONFIG>" CACHE PATH "")
  set(CMI_BINARAY_OUTPUT_DIRECTORY "${CMI_OUTPUT_DIRECTORY}/bin" CACHE PATH "")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMI_BINARAY_OUTPUT_DIRECTORY}" CACHE INTERNAL "")
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMI_BINARAY_OUTPUT_DIRECTORY}" CACHE INTERNAL "")
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMI_BINARAY_OUTPUT_DIRECTORY}" CACHE INTERNAL "")

  mark_as_advanced(CMI_PRESET_FOLDER)
  mark_as_advanced(CMI_OUTPUT_DIRECTORY)
  mark_as_advanced(CMI_BINARAY_OUTPUT_DIRECTORY)
  mark_as_advanced(CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
  mark_as_advanced(CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  mark_as_advanced(CMAKE_RUNTIME_OUTPUT_DIRECTORY)

  set_property(GLOBAL PROPERTY PREDEFINED_TARGETS_FOLDER "${CMI_PRESET_FOLDER}")

  # Set a default build type if none was specified
  set(DEFAULT_BUILD_TYPE_ "Release")
  if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to '${DEFAULT_BUILD_TYPE_}' as none was specified.")
    set(CMAKE_BUILD_TYPE "${DEFAULT_BUILD_TYPE_}" CACHE
        STRING "Choose the type of build." FORCE)
    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
      "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
  endif()
  unset(DEFAULT_BUILD_TYPE_)

  cmi_disable_vs_debug_runtime()
  cmi_fortran_default_mangling()
endmacro()

function(cmi_set_directory TARGET_)
  set(OPTIONS_ OUTPUT IDE)
  cmake_parse_arguments("" "" "${OPTIONS_}" "" ${ARGN})

  if(NOT DEFINED _OUTPUT OR NOT IS_ABSOLUTE "${_OUTPUT}")
    set(_OUTPUT "${CMI_BINARAY_OUTPUT_DIRECTORY}/${_OUTPUT}")
  endif()

  get_filename_component(_OUTPUT "${_OUTPUT}" REALPATH)
  get_filename_component(BINARY_DIR "${CMI_BINARAY_OUTPUT_DIRECTORY}" REALPATH)
  file(RELATIVE_PATH PATH_TO_BIN "${_OUTPUT}" "${BINARY_DIR}")
  if(TARGET ${TARGET_})
    set_property(TARGET ${TARGET_} PROPERTY BUILD_WITH_INSTALL_RPATH TRUE)
    set_property(TARGET ${TARGET_} PROPERTY INSTALL_RPATH "$ORIGIN/${PATH_TO_BIN}")
    set_property(TARGET ${TARGET_} PROPERTY ARCHIVE_OUTPUT_DIRECTORY "${_OUTPUT}")
    set_property(TARGET ${TARGET_} PROPERTY LIBRARY_OUTPUT_DIRECTORY "${_OUTPUT}")
    set_property(TARGET ${TARGET_} PROPERTY RUNTIME_OUTPUT_DIRECTORY "${_OUTPUT}")
    set_property(TARGET ${TARGET_} PROPERTY FOLDER "${PROJECT_NAME}/${_IDE}")
    set_property(GLOBAL PROPERTY USE_FOLDERS ON)
  else()
    message(WARNING "Can not set directoy for unknown target ${TARGET_}")
  endif()
endfunction()

function(cmi_copy TARGET_ SOURCE_ DESTINATION_)
  get_filename_component(SOURCE_DIRECTORY_ "${SOURCE_}" DIRECTORY)
  get_filename_component(SOURCE_NAME_WE_ "${SOURCE_}" NAME_WE)
  get_filename_component(SOURCE_NAME_ "${SOURCE_}" NAME)
  add_custom_target(${TARGET_}
    COMMAND ${CMAKE_COMMAND} -DFILE_PATTERN="${SOURCE_DIRECTORY_}/${SOURCE_NAME_WE_}*" -DDESTINATION="${DESTINATION_}" -P "${CMAKE_CURRENT_LIST_DIR}/cmi_copy.txt"
  )
endfunction()


######################################
# COMPILER FLAGS
######################################

function(cmi_Fortran_append var name)
  # Compiler presets
  set(Fortran_TRACEBACK_Linux_GNU "-fbacktrace")
  set(Fortran_TRACEBACK_CYGWIN_GNU "-fbacktrace")
  set(Fortran_TRACEBACK_Linux_Intel "-traceback")
  set(Fortran_TRACEBACK_Windows_Intel "/traceback")

  set(Fortran_CONSISTENCY_Linux_GNU "")
  set(Fortran_CONSISTENCY_CYGWIN_GNU "")
  set(Fortran_CONSISTENCY_Linux_Intel "-fimf-arch-consistency=true")
  set(Fortran_CONSISTENCY_Windows_Intel "/Qimf-arch-consistency:true")

  set(Fortran_FPP_Linux_GNU "-cpp -ffree-line-length-none -ffixed-line-length-none")
  set(Fortran_FPP_CYGWIN_GNU "-cpp -ffree-line-length-none -ffixed-line-length-none")
  set(Fortran_FPP_Linux_Intel "-fpp -allow nofpp-comments")
  set(Fortran_FPP_Windows_Intel "/fpp")

  set(Fortran_FPE0_Linux_GNU "-ffpe-trap=invalid,zero,overflow")
  set(Fortran_FPP0_CYGWIN_GNU "-ffpe-trap=invalid,zero,overflow")
  set(Fortran_FPE0_Linux_Intel "-fpe0")
  set(Fortran_FPE0_Windows_Intel "/fpe:0")

  set(Fortran_FPSCOMPGENERAL_Linux_GNU "")
  set(Fortran_FPSCOMPGENERAL_CYGWIN_GNU "")
  set(Fortran_FPSCOMPGENERAL_Linux_Intel "-fpscomp general")
  set(Fortran_FPSCOMPGENERAL_Windows_Intel "/fpscomp:general")

  set(Fortran_FPMODELSOURCE_Linux_GNU "")
  set(Fortran_FPMODELSOURCE_CYGWIN_GNU "")
  set(Fortran_FPMODELSOURCE_Linux_Intel "-fp-model source")
  set(Fortran_FPMODELSOURCE_Windows_Intel "/fp:source")

  set(Fortran_FPSPECULATIONSAFE_Linux_GNU "")
  set(Fortran_FPSPECULATIONSAFE_CYGWIN_GNU "")
  set(Fortran_FPSPECULATIONSAFE_Linux_Intel "-fp-speculation safe")
  set(Fortran_FPSPECULATIONSAFE_Windows_Intel "/Qfp-speculation=safe")

  set(Fortran_OMP_Linux_GNU "-fopenmp")
  set(Fortran_OMP_CYGWIN_GNU "-fopenmp")
  set(Fortran_OMP_Linux_Intel "-qopenmp")
  set(Fortran_OMP_Windows_Intel "/Qopenmp")

  set(Fortran_TRAPUV_Linux_GNU "-finit-real=snan -finit-integer=-1 -finit-character=0 -finit-logical=false")
  set(Fortran_TRAPUV_CYGWIN_GNU "-finit-real=snan -finit-integer=-1 -finit-character=0 -finit-logical=false")
  set(Fortran_TRAPUV_Linux_Intel "-ftrapuv")
  set(Fortran_TRAPUV_Windows_Intel "/Qtrapuv")

  set(Fortran_WSRCTRUNC_Linux_GNU "-Wline-truncation")
  set(Fortran_WSRCTRUNC_CYGWIN_GNU "-Wline-truncation")
  set(Fortran_WSRCTRUNC_Linux_Intel "-warn truncated_source")
  set(Fortran_WSRCTRUNC_Windows_Intel "/warn:truncated_source")

  set(Fortran_CHECKALL_Linux_GNU "-fcheck=pointer,bounds")
  set(Fortran_CHECKALL_CYGWIN_GNU "-fcheck=pointer,bounds")
  set(Fortran_CHECKALL_Linux_Intel "-check pointer,bounds,uninit,format,output_conversion")
  set(Fortran_CHECKALL_Windows_Intel "/check:pointer /check:bounds /check:uninit /check:format /check:output_conversion")

  set(Fortran_THREADS_Linux_GNU "-pthread")
  set(Fortran_THREADS_CYGWIN_GNU "-pthread")
  set(Fortran_THREADS_Linux_Intel "-threads")
  set(Fortran_THREADS_Windows_Intel "/threads")

  set(Fortran_STACK_Linux_GNU "")
  set(Fortran_STACK_CYGWIN_GNU "")
  set(Fortran_STACK_Linux_Intel "")
  set(Fortran_STACK_Windows_Intel "/STACK:10000000,10000000")

  set(${var} "${${var}} ${Fortran_${name}_${CMAKE_SYSTEM_NAME}_${CMAKE_Fortran_COMPILER_ID}}" PARENT_SCOPE)
  string(STRIP "${${var}}" ${var})
endfunction()

function(cmi_fortran_default_mangling)
  set(flag "/names:lowercase /assume:underscore")
  if(WIN32)
    string(FIND "${CMAKE_Fortran_FLAGS}" "${flag}" status)
    if("${status}" STREQUAL "-1")
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${flag}" PARENT_SCOPE)
    endif()
  endif()
endfunction()

function(cmi_disable_vs_debug_runtime)
  set(LANGUAGES "C" "CXX" "Fortran")
  foreach(LANGUAGE IN LISTS LANGUAGES)
    set(TARGET_FLAG "CMAKE_${LANGUAGE}_FLAGS_DEBUG")
    if(DEFINED ${TARGET_FLAG})
      set(FLAGS "${${TARGET_FLAG}}")
      string(REPLACE "/dbglibs" "" FLAGS "${FLAGS}")
      string(REPLACE "/MDd" "/MD" FLAGS "${FLAGS}")
      string(REPLACE "/MTd" "/MT" FLAGS "${FLAGS}")
      set(${TARGET_FLAG} "${FLAGS}" PARENT_SCOPE)
    endif()
  endforeach()
endfunction()


######################################
# IMPORT
######################################

function(cmi_add_external_library LIB_NAME_ LIB_SOURCE_ LIB_DESTINATION_)
  set(SCRIPT_FILE_ "${CMAKE_CURRENT_BINARY_DIR}/${LIB_NAME_}_import.$<CONFIG>.cmake")
  find_library(LIBS_ NAMES ${LIB_NAME_} lib${LIB_NAME_} HINTS ${LIB_SOURCE_} NO_DEFAULT_PATH)
  get_filename_component(LIB_SOURCE_DIRECTORY "${LIBS_}" DIRECTORY)
  get_filename_component(LIB_SOURCE_NAME_WE "${LIBS_}" NAME_WE)
  get_filename_component(LIB_SOURCE_NAME "${LIBS_}" NAME)
  file(GLOB LIB_FILES "${LIB_SOURCE_DIRECTORY}/${LIB_SOURCE_NAME_WE}*")
  unset(LIBS_ CACHE)

  # Generate copy script
  file(GENERATE OUTPUT ${SCRIPT_FILE_} CONTENT "
  set(TARGET_DIRECTORY \"${LIB_DESTINATION_}\")
  set(SOURCE_FILES \"${LIB_FILES}\")
  foreach(FILE_ IN LISTS SOURCE_FILES)
    get_filename_component(FILE_NAME_ \"\${FILE_}\" NAME)
    set(FILE_TARGET_DIRECTORY_ \"\${TARGET_DIRECTORY}/\${FILE_NAME_}\")
    if(EXISTS \${FILE_TARGET_DIRECTORY_})
      file(TIMESTAMP \${FILE_} FILE_TIMESTAMP_)
      file(TIMESTAMP \${FILE_TARGET_DIRECTORY_} FILE_TARGET_DIRECTORY_TIMESTAMP_)
      if(FILE_TIMESTAMP_ STREQUAL FILE_TARGET_DIRECTORY_TIMESTAMP_)
        continue()
      endif()
    endif()
    message(STATUS \"Updating \${TARGET_DIRECTORY}/\${FILE_NAME_}\")
    file(COPY \${FILE_} DESTINATION \${TARGET_DIRECTORY})
  endforeach()
  ")

  # Generate copy target
  add_custom_target(${LIB_NAME_}_import COMMAND ${CMAKE_COMMAND} -P "${SCRIPT_FILE_}")

  # Generate interface target
  add_library(${LIB_NAME_} INTERFACE IMPORTED GLOBAL)
  target_link_libraries(${LIB_NAME_} INTERFACE "${LIB_DESTINATION_}/${LIB_SOURCE_NAME}")
  add_dependencies(${LIB_NAME_} ${LIB_NAME_}_import)
  cmi_set_directory(${LIB_NAME_}_import IDE "imports")
endfunction()


######################################
# FIND
######################################

# On Linux: User must provide MPI wrapper. This is the only way to get Fortran MPI modules work right, when switching between ifort and gfortran.
# On Windows: Since MPI wrapper are not available/broken, we rely on cmake's findMPI
# Usage(Windows/Linux):
# target_include_directories(mytarget PUBLIC "${MPI_INCLUDE}")
# target_link_libraries(mytarget PUBLIC "${MPI_LIB}")
# Setting up MPI
macro(cmi_find_mpi)
  if(WIN32)
    # Don't use FindMPI when using MS Visual Studio
    foreach(COMPILER_ IN ITEMS C CXX Fortran)
      if(DEFINED CMAKE_${COMPILER_}_COMPILER)
        if(MPI_${COMPILER_}_FOUND)
          set(MPI_FOUND TRUE CACHE INTERNAL "")
        else()
          set(MPI_FOUND FALSE CACHE INTERNAL "")
          break()
        endif()
      endif()
    endforeach()
    unset(COMPILER_)

    if(NOT MPI_FOUND)

      if(NOT DEFINED I_MPI_ROOT)
        set(CMPI_ROOT "$ENV{I_MPI_ROOT}")
      else()
        set(CMPI_ROOT "${I_MPI_ROOT}")
      endif()

      if(NOT EXISTS "${CMPI_ROOT}")
        message(STATUS " Folder does NOT exist: I_MPI_ROOT=${CMPI_ROOT}")
      else()
        set(MPI_INCLUDE "${CMPI_ROOT}/intel64/include" CACHE PATH "")
        set(MPI_LIB "${CMPI_ROOT}/intel64/lib/release/impi.lib" CACHE PATH "")
        set(MPIEXEC "${CMPI_ROOT}/intel64/bin/mpiexec.exe" CACHE PATH "")
        message(STATUS " I_MPI_ROOT: " "${CMPI_ROOT}")

        if(NOT EXISTS "${MPI_INCLUDE}")
          message(STATUS " MPI_INCLUDE path does NOT exist: ${MPI_INCLUDE}")
        else()
          message(STATUS " MPI_INCLUDE: ${MPI_INCLUDE}")

          if(NOT EXISTS "${MPI_LIB}")
            message(STATUS " MPI_LIB path does not exist: ${MPI_LIB}")
          else()
            message(STATUS " MPI_LIB: ${MPI_LIB}")

            if(NOT EXISTS "${MPIEXEC}")
              message(STATUS " MPIEXEC does not exist: ${MPIEXEC}")
            else()
              message(STATUS " MPIEXEC: ${MPIEXEC}")

              # Check C and CXX
              if(CMAKE_C_COMPILER OR CMAKE_CXX_COMPILER)
                if(NOT EXISTS "${MPI_INCLUDE}/mpi.h")
                  message(STATUS " Could NOT find mpi.h file in: ${MPI_INCLUDE}")
                else()
                  if(CMAKE_C_COMPILER)
                    set(MPI_C_FOUND TRUE CACHE INTERNAL "")
                  endif()
                  if(CMAKE_CXX_COMPILER)
                    set(MPI_CXX_FOUND TRUE CACHE INTERNAL "")
                  endif()
                endif()
              endif()

              # Check Fortran
              if(CMAKE_Fortran_COMPILER)
                if(NOT EXISTS "${MPI_INCLUDE}/mpif.h")
                  message(STATUS " Could NOT find mpif.h file in: ${MPI_INCLUDE}")
                else()
                  if(CMAKE_Fortran_COMPILER)
                    set(MPI_Fortran_FOUND TRUE CACHE INTERNAL "")
                  endif()
                endif()
              endif()

            endif()
          endif()
        endif()

      endif()

      foreach(COMPILER_ IN ITEMS C CXX Fortran)
        if(DEFINED CMAKE_${COMPILER_}_COMPILER)
          if(MPI_${COMPILER_}_FOUND)
            set(MPI_FOUND TRUE CACHE INTERNAL "")
          else()
            set(MPI_FOUND FALSE CACHE INTERNAL "")
            message("Missing ${COMPILER_}")
            break()
          endif()
        endif()
      endforeach()
      unset(COMPILER_)

      if(DEFINED MPI_FOUND AND NOT MPI_FOUND)
        message(WARNING " Make sure that IntelMPI SDK is installed and env var I_MPI_ROOT points to it. For example:\n"
                        " I_MPI_ROOT=C:\\Program Files(x86)\\IntelSWTools\\compilers_and_libraries_2019.5.281\\windows\\mpi\\\n")
      endif()
    endif()
  else()
    if(DEFINED CMAKE_C_COMPILER)
      set(MPI_C_COMPILER ${CMAKE_C_COMPILER} CACHE FILEPATH "")
    endif()
    if(DEFINED CMAKE_CXX_COMPILER)
      set(MPI_CXX_COMPILER ${CMAKE_CXX_COMPILER} CACHE FILEPATH "")
    endif()
    if(DEFINED CMAKE_Fortran_COMPILER)
      set(MPI_Fortran_COMPILER ${CMAKE_Fortran_COMPILER} CACHE FILEPATH "")
    endif()

    find_package(MPI)
    if(CMAKE_C_COMPILER)
      if(NOT MPI_C_FOUND)
        message(STATUS "C compiler ${CMAKE_C_COMPILER} has NO MPI support.\nTo enable support, provide MPI compiler wrapper explicitly e.g. 'CC=mpicc cmake', delete cache and rerun cmake.")
      endif()
    endif()
    if(CMAKE_CXX_COMPILER)
      if(NOT MPI_CXX_FOUND)
        message(STATUS "CXX compiler ${CMAKE_CXX_COMPILER} has NO MPI support.\nTo enable support, provide MPI compiler wrapper explicitly e.g. 'CXX=mpicxx cmake', delete cache and rerun cmake.")
      endif()
    endif()
    if(CMAKE_Fortran_COMPILER)
      if(NOT MPI_Fortran_FOUND)
        message(STATUS "Fortran compiler ${CMAKE_Fortran_COMPILER} has NO MPI support.\nTo enable support, provide MPI compiler warpper explicitly e.g. 'FC=mpifc cmake', delete cache and rerun cmake.")
      endif()
    endif()
    set(MPI_INCLUDE "" CACHE PATH "")
    set(MPI_LIB "" CACHE PATH "")
  endif()

  foreach(COMPILER_ IN ITEMS C CXX Fortran)
    set(CMPI_${COMPILER_}_FOUND ${MPI_${COMPILER_}_FOUND})
    if(MPI_${COMPILER_}_FOUND)
      set(MPI_FOUND TRUE)
    endif()
  endforeach()
  unset(COMPILER_)

  unset(CMPI_ROOT)
  unset(CMPI_FOUND)
  unset(CMPI_C_FOUND)
  unset(CMPI_CXX_FOUND)
  unset(CMPI_Fortran_FOUND)

endmacro()


macro(cmi_find_python_interpreter_)
  if(${CMAKE_VERSION} VERSION_LESS "3.12.0")
    # Backwards compatibility
    find_package(PythonInterp REQUIRED)
    set(Python_EXECUTABLE "${PYTHON_EXECUTABLE}")
  else()
    find_package(Python REQUIRED Interpreter)
  endif()
endmacro()

macro(cmi_find_python_development_)
  if(${CMAKE_VERSION} VERSION_LESS "3.12.0")
    # Backwards compatibility
    find_package(PythonLibs REQUIRED)
    set(Python_INCLUDE_DIRS "${PYTHON_INCLUDE_DIRS}")
  else()
    find_package(Python REQUIRED Development)
  endif()
endmacro()

macro(cmi_find_python)
  set(OPTIONS_ PACKAGES COMPONENTS)
  cmake_parse_arguments("" "" "" "${OPTIONS_}" ${ARGN})

  if(DEFINED _COMPONENTS)
    foreach(_COMPONENT IN LISTS _COMPONENTS)
      if(_COMPONENT STREQUAL "interpreter")
        cmi_find_python_interpreter_()
      elseif(_COMPONENT STREQUAL "development")
        cmi_find_python_development_()
      endif()
    endforeach()
  else()
    cmi_find_python_interpreter_()
  endif()
  unset(_COMPONENT)
  unset(_COMPONENTS)


  foreach(PACKAGE_ IN LISTS _PACKAGES)
    if(NOT CMI_Python_${PACKAGE_}_FOUND)
      set(CMI_PYTHON_PACKAGE_MESSAGE "Check for python package ${PACKAGE_}")
      message(STATUS "${CMI_PYTHON_PACKAGE_MESSAGE}")
      execute_process(COMMAND "${Python_EXECUTABLE}" "-c" "import ${PACKAGE_}"
                      RESULT_VARIABLE PYTHON_RESULT
                      OUTPUT_QUIET
                      ERROR_QUIET)
      if(${PYTHON_RESULT} STREQUAL 0)
        message(STATUS "${CMI_PYTHON_PACKAGE_MESSAGE} - found")
        set(CMI_Python_${PACKAGE_}_FOUND 1 CACHE INTERNAL "")
      else()
        message(SEND_ERROR "${CMI_PYTHON_PACKAGE_MESSAGE} - not found")
        set(CMI_Python_${PACKAGE_}_FOUND 0 CACHE INTERNAL "")
      endif()
    endif()
  endforeach()
  unset(CMI_PYTHON_PACKAGE_MESSAGE)
  unset(PACKAGE_)
  unset(PACKAGES_)
endmacro()


function(cmi_find_omp)
  set(OMP_DIRECTIVE_ "_OMP")
  cmi_Fortran_append(OMP_FLAGS_ OMP)
  string(STRIP "${OMP_FLAGS_}" OMP_FLAGS_)
  if(NOT TARGET OpenMP_Fortran)
    add_library(OpenMP_Fortran IMPORTED INTERFACE)
    set_property(TARGET OpenMP_Fortran PROPERTY INTERFACE_COMPILE_OPTIONS ${OMP_FLAGS_})
    set_property(TARGET OpenMP_Fortran PROPERTY INTERFACE_COMPILE_DEFINITIONS ${OMP_DIRECTIVE_})
    set_property(TARGET OpenMP_Fortran PROPERTY INTERFACE_LINK_LIBRARIES ${OMP_FLAGS_})
  endif()
endfunction()

