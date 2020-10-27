# BSD 2-Clause License
#
# Copyright (c) 2019-2020, Volker Jacht
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# See https://gitlab.com/nordfox/cmakeit

cmake_minimum_required(VERSION 3.8)

set(CMI_TAG "9830c79124380fa4958a61f37807948dc48961dc")

get_property(CMI_LOADER_FILE GLOBAL PROPERTY CMI_LOADER_FILE)
# First include
if(NOT CMI_LOADER_FILE)
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

    #Check filesize of download
    set(INPUT_FILE_ "${CMI_LOADER_TMP_}.in")
    if(${CMAKE_VERSION} VERSION_LESS "3.14.0")
      set(CMI_LOADER_FILESIZE_ "0")
      if(EXISTS "${INPUT_FILE_}")
        file(READ "${INPUT_FILE_}" FILE_CONTENT_ LIMIT 1 HEX)
        string(LENGTH "${FILE_CONTENT_}" CMI_LOADER_FILESIZE_)
        math(EXPR CMI_LOADER_FILESIZE_ "${CMI_LOADER_FILESIZE_} / 2")
      endif()
      unset(FILE_CONTENT_)
    else()
      file(SIZE "${INPUT_FILE_}" CMI_LOADER_FILESIZE_)
    endif()
    unset(INPUT_FILE_)
    ###

    if(NOT CMI_LOADER_FILESIZE_ STREQUAL 0)
      configure_file("${CMI_LOADER_TMP_}.in" "${CMI_LOADER_TMP_}" @ONLY NEWLINE_STYLE UNIX)
      execute_process(
        COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${CMI_LOADER_TMP_}" "${CMAKE_CURRENT_LIST_FILE}"
      )
      file(REMOVE "${CMI_LOADER_TMP_}.in" "${CMI_LOADER_TMP_}")
      include("${CMAKE_CURRENT_LIST_FILE}")
      unset(CMI_FILENAME_)
      unset(CMI_LOADER_TMP_)
      return()
    else()
      message(WARNING "Updating CMI failed! Using existing version.")
    endif()
  endif()
  set_property(GLOBAL PROPERTY CMI_LOADER_FILE "${CMAKE_CURRENT_LIST_FILE}")
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

######################################
# ACTUAL CMI FILE BEGINS
######################################

set(CMI_LOADED TRUE)
set(CMI_VERSION "0.1.0")

# Consider root project as populated and included
string(TOUPPER "${CMAKE_PROJECT_NAME}" PROJECT_NAME_UPPER_)
set_property(GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_INCLUDED "TRUE")
unset(PROJECT_NAME_UPPER_)

set(CMI_EXTERNALS_DIR "${CMAKE_SOURCE_DIR}/_externals" CACHE PATH "")
set(CMI_IDE_PRESETS "_targets" CACHE PATH "")
set(CMI_IDE_EXTERNALS "_externals" CACHE PATH "")
set(CMI_CHECKOUT_SCRIPT "${CMAKE_BINARY_DIR}/checkout.cmake")

option(CMI_AUTO_CHECKOUT "" ON)

mark_as_advanced(CMI_EXTERNALS_DIR)
mark_as_advanced(CMI_IDE_PRESETS)
mark_as_advanced(CMI_IDE_EXTERNALS)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set_property(GLOBAL PROPERTY PREDEFINED_TARGETS_FOLDER "${CMI_IDE_PRESETS}")

######################################
# HELPER FUNCTIONS
######################################

function(cmi_minimum_required)
  cmake_parse_arguments("" "" "VERSION" "" ${ARGN})
  if("${CMI_VERSION}" VERSION_LESS "${_VERSION}")
    message(FATAL_ERROR "CMI ${_VERSION} or higher is required. You are running version ${CMI_VERSION}")
  endif()
endfunction()

function(cmi_file)
  cmake_parse_arguments(_FILE "" "" "SIZE" ${ARGN})
  list(LENGTH _FILE_SIZE _FILE_LENGTH)
  if (_FILE_LENGTH STREQUAL 2)
    list(GET _FILE_SIZE 0 INPUT_FILE)
    list(GET _FILE_SIZE 1 FILE_SIZE)
    if(${CMAKE_VERSION} VERSION_LESS "3.14.0")
      set(FILE_SIZE_ "0")
      if(EXISTS "${INPUT_FILE}")
        file(READ "${INPUT_FILE}" FILE_CONTENT_ HEX)
        string(LENGTH "${FILE_CONTENT_}" FILE_SIZE_)
        math(EXPR FILE_SIZE_ "${FILE_SIZE_} / 2")
      endif()
    else()
      file(SIZE "${INPUT_FILE}" FILE_SIZE_)
    endif()
    set(${FILE_SIZE} ${FILE_SIZE_} PARENT_SCOPE)
  endif()
endfunction()


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
  set(CMI_ENABLE_PREFIX "CMI_ENABLE_")
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
      get_property(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE GLOBAL PROPERTY CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE)
      get_property(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL GLOBAL PROPERTY CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL)
      get_property(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_REV GLOBAL PROPERTY CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_REV)
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

# Only set property if it hasnt been set before
macro(cmi_set_new_property_ PROPERTY_NAME_ PROPERTY_VALUE_)
  get_property(OLD_VALUE_ GLOBAL PROPERTY ${PROPERTY_NAME_})
  if("${OLD_VALUE_}" STREQUAL "")
    set_property(GLOBAL PROPERTY ${PROPERTY_NAME_} ${PROPERTY_VALUE_})
  endif()
endmacro()

function(cmi_add_update_script_ FILEPATH_)
  set_property(GLOBAL APPEND PROPERTY CMI_UPDATE_LIST ${FILEPATH_})
  get_property(TEST_ GLOBAL PROPERTY CMI_UPDATE_LIST)

  set(UPDATER_ "${CMAKE_BINARY_DIR}/checkout.cmake")

  file(WRITE "${UPDATER_}" "")
  file(APPEND "${UPDATER_}" "
    set(files \n")
  foreach(file_ IN LISTS TEST_)
    file(APPEND "${UPDATER_}" "      \"${file_}\"\n")
  endforeach()
  if(CMAKE_GENERATOR MATCHES "Visual Studio")
    set(USE_ZERO_CHECK_ 1)
  else()
    set(USE_ZERO_CHECK_ 0)
  endif()
  file(APPEND "${UPDATER_}" "    )
    foreach(file_ IN LISTS files)
      execute_process(COMMAND \"${CMAKE_COMMAND}\" -P \"\${file_}\" OUTPUT_VARIABLE OUTPUT_)
      string(REGEX REPLACE \"\\n\$\" \"\" OUTPUT_ \"\${OUTPUT_}\")
      if(OUTPUT_)
        message(STATUS \"Processing \${file_}\n\${OUTPUT_}\")
      endif()
      if(OUTPUT_ MATCHES \"Update: \")
        message(\"STATUS SUCCESS \${file_}\")
        if(${USE_ZERO_CHECK_})
          execute_process(
            COMMAND \${CMAKE_COMMAND} --build \"${CMAKE_BINARY_DIR}\" --target ZERO_CHECK
          )
        else()
          execute_process(
            COMMAND \${CMAKE_COMMAND} \"${CMAKE_BINARY_DIR}\"
          )
        endif()
      endif()
    endforeach()
  ")

  if(NOT TARGET checkout)
    add_custom_target(checkout ${CMAKE_COMMAND} -P "${CMI_CHECKOUT_SCRIPT}")
    set_property(TARGET checkout PROPERTY FOLDER "${CMI_IDE_EXTERNALS}")
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
  cmi_set_new_property_(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE "git")
  cmi_set_new_property_(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL "${PROJECT_URL_}")
  cmi_set_new_property_(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_REV "${PROJECT_REV_}")
endfunction()

function(cmi_add_svn PROJECT_NAME_ PROJECT_URL_ PROJECT_REV_)
  string(TOUPPER "${PROJECT_NAME_}" PROJECT_NAME_UPPER_)
  cmi_set_new_property_(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_TYPE "svn")
  cmi_set_new_property_(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_URL "${PROJECT_URL_}")
  cmi_set_new_property_(CMI_${PROJECT_NAME_UPPER_}_EXTERNAL_REV "${PROJECT_REV_}")
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
      cmi_file(SIZE "${${PROJECT_NAME_UPPER_}_ARCHIVE_PATH}" ARCHIVE_FILESIZE_)
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


function(cmi_add_git_ PROJECT_NAME_ PROJECT_URL_ PROJECT_REV_)
  string(TOUPPER "${PROJECT_NAME_}" PROJECT_NAME_UPPER_)
  string(TOLOWER "${PROJECT_NAME_}" PROJECT_NAME_LOWER_)
  get_property(${PROJECT_NAME_UPPER_}_POPULATED GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_POPULATED)
  if(${PROJECT_NAME_UPPER_}_POPULATED)
    return()
  endif()
  set_property(GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_POPULATED "TRUE")

  find_package(Git)
  set(${PROJECT_NAME_UPPER_}_DIR "${CMI_EXTERNALS_DIR}/${PROJECT_NAME_LOWER_}" CACHE PATH "")
  set(UPDATER_ "${CMAKE_BINARY_DIR}/checkout.${PROJECT_NAME_LOWER_}.cmake")

  cmi_add_update_script_("${UPDATER_}")

  # Generate copy script
  file(WRITE "${UPDATER_}" "
    set(GIT_EXECUTABLE \"${GIT_EXECUTABLE}\")
    set(GIT_DIR \"${${PROJECT_NAME_UPPER_}_DIR}\")
    set(GIT_URL \"${PROJECT_URL_}\")
    set(GIT_REV \"${PROJECT_REV_}\")

    set(GIT_FETCH 1)

    if(EXISTS \"\${GIT_DIR}\")
      execute_process(
        COMMAND \${GIT_EXECUTABLE} rev-list -n 1 \${GIT_REV}
        WORKING_DIRECTORY \"\${GIT_DIR}\"
        RESULT_VARIABLE RESULT_
        ERROR_QUIET
        OUTPUT_VARIABLE TARGET_REV_
      )
      if(RESULT_ STREQUAL \"0\" AND NOT (\${GIT_REV} MATCHES \"^origin/.+\"))
        set(GIT_FETCH 0)
      endif()
    endif()

    # Clone repo
    if(GIT_FETCH)
      foreach(try RANGE 2)
        if(NOT EXISTS \"\${GIT_DIR}\")
          message(STATUS \"Clone \${GIT_DIR} \${GIT_URL} \${GIT_REV}\")
          set(TMP_DIR \"\${GIT_DIR}.tmp\")
          if(EXISTS \"\${TMP_DIR}\")
            message(STATUS \"Removing \${TMP_DIR}\")
            file(REMOVE_RECURSE \"\${TMP_DIR}\")
          endif()
          execute_process(
            COMMAND \${GIT_EXECUTABLE} clone \${GIT_URL} \"\${TMP_DIR}\"
            RESULT_VARIABLE RESULT_ ERROR_VARIABLE STDERR_
          )
          if(RESULT_ STREQUAL 0)
            file(RENAME \"\${TMP_DIR}\" \"\${GIT_DIR}\")
            break()
          endif()
          # Cloning failed. Delete directory.
          file(REMOVE_RECURSE \"\${TMP_DIR}\")
        else()
          message(STATUS \"Fetch \${GIT_DIR} \${GIT_URL} \${GIT_REV}\")
          execute_process(
            COMMAND \${GIT_EXECUTABLE} fetch
            WORKING_DIRECTORY \"\${GIT_DIR}\"
            RESULT_VARIABLE RESULT_ ERROR_VARIABLE STDERR_
          )
          if(RESULT_ STREQUAL 0)
            break()
          endif()
        endif()
        message(SEND_ERROR \"Git clone/fetch failed.\")
        if(\${STDERR_} MATCHES \"bad line length character: git\")
          message(WARNING \"Probably missing SSH private key!\")
          return()
        endif()
      endforeach()
    endif()

    # Git Checkout
    # message(STATUS \"Checkout \${GIT_REV}\")
    execute_process(
      COMMAND \${GIT_EXECUTABLE} status -s
      WORKING_DIRECTORY \"\${GIT_DIR}\"
      RESULT_VARIABLE RESULT_
      OUTPUT_VARIABLE STASH_NEEDED_
    )
    execute_process(
      COMMAND \${GIT_EXECUTABLE} rev-list -n 1 HEAD
      WORKING_DIRECTORY \"\${GIT_DIR}\"
      RESULT_VARIABLE RESULT_
      OUTPUT_VARIABLE CURRENT_REV_
    )
    execute_process(
      COMMAND \${GIT_EXECUTABLE} rev-list -n 1 \${GIT_REV}
      WORKING_DIRECTORY \"\${GIT_DIR}\"
      RESULT_VARIABLE RESULT_
      OUTPUT_VARIABLE TARGET_REV_
      ERROR_QUIET
    )

    if(STASH_NEEDED_ OR (NOT CURRENT_REV_ STREQUAL TARGET_REV_))
      message(STATUS \"Update: \${GIT_DIR}\")
    endif()

    if(NOT RESULT_ STREQUAL \"0\")
      message(FATAL_ERROR \"Rev \${GIT_REV} not found.\")
    endif()

    if(STASH_NEEDED_)
      execute_process(
        COMMAND \${GIT_EXECUTABLE} stash
        WORKING_DIRECTORY \"\${GIT_DIR}\"
        RESULT_VARIABLE RESULT_
        OUTPUT_VARIABLE STDOUT_
        ERROR_VARIABLE STDOUT_
      )
      message(STATUS \"\${STDOUT_}\")
    endif()
    if(NOT CURRENT_REV_ STREQUAL TARGET_REV_)
      execute_process(
        COMMAND \${GIT_EXECUTABLE} -c advice.detachedHead=false checkout \${GIT_REV}
        WORKING_DIRECTORY \"\${GIT_DIR}\"
        RESULT_VARIABLE RESULT_
        OUTPUT_VARIABLE STDOUT_
        ERROR_VARIABLE STDOUT_
      )
      message(STATUS \"\${STDOUT_}\")
    endif()

    if(NOT (RESULT_ STREQUAL 0))
      message(FATAL_ERROR \"Git checkout failed.\")
    endif()
  ")
  if(CMI_AUTO_CHECKOUT OR NOT EXISTS "${${PROJECT_NAME_UPPER_}_DIR}")
    execute_process(
      COMMAND ${CMAKE_COMMAND} -P "${UPDATER_}"
      RESULT_VARIABLE RESULT_
    )
  endif()
  if(NOT RESULT_ STREQUAL "0")
    message(SEND_ERROR "Could not update ${GIT_DIR}")
  endif()
endfunction()

function(cmi_add_svn_ PROJECT_NAME_ PROJECT_URL_ PROJECT_REV_)
  string(TOUPPER "${PROJECT_NAME_}" PROJECT_NAME_UPPER_)
  string(TOLOWER "${PROJECT_NAME_}" PROJECT_NAME_LOWER_)
  get_property(${PROJECT_NAME_UPPER_}_POPULATED GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_POPULATED)
  if(${PROJECT_NAME_UPPER_}_POPULATED)
    return()
  endif()
  set_property(GLOBAL PROPERTY ${PROJECT_NAME_UPPER_}_POPULATED "TRUE")

  find_package(Subversion)
  set(${PROJECT_NAME_UPPER_}_DIR "${CMI_EXTERNALS_DIR}/${PROJECT_NAME_LOWER_}" CACHE PATH "")


  set(UPDATER_ "${CMI_EXTERNALS_DIR}/checkout.${PROJECT_NAME_LOWER_}.cmake")

  cmi_add_update_script_("${UPDATER_}")

  # Generate copy script
  file(WRITE "${UPDATER_}" "
    set(SVN_EXECUTABLE \"${Subversion_SVN_EXECUTABLE}\")
    set(SVN_DIR \"${${PROJECT_NAME_UPPER_}_DIR}\")
    set(SVN_URL \"${PROJECT_URL_}\")
    set(SVN_REV \"${PROJECT_REV_}\")


    if(NOT EXISTS \"\${SVN_DIR}\")
      foreach(try RANGE 2)
        message(STATUS \"Checkout \${SVN_DIR} \${SVN_URL} \${SVN_REV}\")
        set(TMP_DIR \"\${SVN_DIR}.tmp\")
        if(EXISTS \"\${TMP_DIR}\")
          message(STATUS \"Removing \${TMP_DIR}\")
          file(REMOVE_RECURSE \"\${TMP_DIR}\")
        endif()
        execute_process(
          COMMAND \"\${SVN_EXECUTABLE}\" \"checkout\" \"--ignore-externals\" \"\${SVN_URL}@\${SVN_REV}\" \"\${TMP_DIR}\"
          RESULT_VARIABLE RESULT_
        )
        if(RESULT_ STREQUAL 0)
          file(RENAME \"\${TMP_DIR}\" \"\${SVN_DIR}\")
          break()
        endif()
        # Cloning failed. Delete directory.
        message(SEND_ERROR \"SVN checkout failed.\")
        file(REMOVE_RECURSE \"\${TMP_DIR}\")
      endforeach()
    else()
      # SVN Switch
      foreach(try RANGE 2)
        message(STATUS \"Switch \${SVN_DIR} \${SVN_URL} \${SVN_REV}\")
        execute_process(
          COMMAND \"\${SVN_EXECUTABLE}\" \"switch\" \"--ignore-externals\" \"\${SVN_URL}@\${SVN_REV}\" \"\${SVN_DIR}\"
          RESULT_VARIABLE RESULT_
        )
        if(RESULT_ STREQUAL 0)
          break()
        endif()
        message(SEND_ERROR \"SVN switch failed.\")
      endforeach()
    endif()
  ")
  if(NOT EXISTS "${${PROJECT_NAME_UPPER_}_DIR}")
    execute_process(COMMAND ${CMAKE_COMMAND} -P "${UPDATER_}")
  endif()
endfunction()



######################################
# COMPILER FLAGS
######################################

function(cmi_Fortran_append var name)
  # Compiler presets
  set(Fortran_O0_Generic_GNU "-Og")
  set(Fortran_O0_Generic_Intel "-O0")
  set(Fortran_O0_Windows_Intel "/Od")

  set(Fortran_O1_Generic_GNU "-O1")
  set(Fortran_O1_Generic_Intel "-O1")
  set(Fortran_O1_Windows_Intel "/O1")

  set(Fortran_O2_Generic_GNU "-O2")
  set(Fortran_O2_Generic_Intel "-O2")
  set(Fortran_O2_Windows_Intel "/O2")

  set(Fortran_O3_Generic_GNU "-O3")
  set(Fortran_O3_Generic_Intel "-O3")
  set(Fortran_O3_Windows_Intel "/O3")

  set(Fortran_TRACEBACK_Generic_GNU "-fbacktrace")
  set(Fortran_TRACEBACK_Generic_Intel "-traceback")
  set(Fortran_TRACEBACK_Windows_Intel "/traceback")

  set(Fortran_CONSISTENCY_Generic_GNU "")
  set(Fortran_CONSISTENCY_Generic_Intel "-fimf-arch-consistency=true")
  set(Fortran_CONSISTENCY_Windows_Intel "/Qimf-arch-consistency:true")

  set(Fortran_CPP_Generic_GNU "-cpp")
  set(Fortran_CPP_Generic_Intel "-fpp")
  set(Fortran_CPP_Windows_Intel "/fpp")

  set(Fortran_FPP_Generic_GNU "-cpp -ffree-line-length-none -ffixed-line-length-none")
  set(Fortran_FPP_Generic_Intel "-fpp -allow nofpp-comments")
  set(Fortran_FPP_Windows_Intel "/fpp")

  set(Fortran_FPE0_Generic_GNU "-ffpe-trap=invalid,zero,overflow")
  set(Fortran_FPE0_Generic_Intel "-fpe0")
  set(Fortran_FPE0_Windows_Intel "/fpe:0")

  set(Fortran_FPMODELSOURCE_Generic_GNU "")
  set(Fortran_FPMODELSOURCE_Generic_Intel "-fp-model source")
  set(Fortran_FPMODELSOURCE_Windows_Intel "/fp:source")

  set(Fortran_FPSPECULATIONOFF_Generic_GNU "")
  set(Fortran_FPSPECULATIONOFF_Generic_Intel "-fp-speculation off")
  set(Fortran_FPSPECULATIONOFF_Windows_Intel "/Qfp-speculation=off")

  set(Fortran_FPSPECULATIONSAFE_Generic_GNU "")
  set(Fortran_FPSPECULATIONSAFE_Generic_Intel "-fp-speculation safe")
  set(Fortran_FPSPECULATIONSAFE_Windows_Intel "/Qfp-speculation=safe")

  set(Fortran_FPMODELSTRICT_Generic_GNU "")
  set(Fortran_FPMODELSTRICT_Generic_Intel "-fp-model strict")
  set(Fortran_FPMODELSTRICT_Windows_Intel "/fp:strict")

  set(Fortran_OMP_Generic_GNU "-fopenmp")
  set(Fortran_OMP_Generic_Intel "-qopenmp")
  set(Fortran_OMP_Windows_Intel "/Qopenmp")

  set(Fortran_WSRCTRUNC_Generic_GNU "-Wline-truncation")
  set(Fortran_WSRCTRUNC_Generic_Intel "-warn truncated_source")
  set(Fortran_WSRCTRUNC_Windows_Intel "/warn:truncated_source")

  set(Fortran_EXTENDEDLINESF90_Generic_GNU "-ffree-line-length-none")
  set(Fortran_EXTENDEDLINESF90_Generic_Intel "")
  set(Fortran_EXTENDEDLINESF90_Windows_Intel "")

  set(Fortran_WUNUSED_Generic_GNU "-Wunused")
  set(Fortran_WUNUSED_Generic_Intel "-warn unused")
  set(Fortran_WUNUSED_Windows_Intel "/warn:unused")

  set(Fortran_WINTERFACES_Generic_GNU "")
  set(Fortran_WINTERFACES_Generic_Intel "-warn interfaces")
  set(Fortran_WINTERFACES_Windows_Intel "/warn:interfaces")

  set(Fortran_WDECLARATIONS_Generic_GNU "")
  set(Fortran_WDECLARATIONS_Generic_Intel "-warn declarations")
  set(Fortran_WDECLARATIONS_Windows_Intel "/warn:declarations")

  set(Fortran_TRAPUV_Generic_GNU "-finit-real=snan -finit-integer=-1 -finit-character=0 -finit-logical=false")
  set(Fortran_TRAPUV_Generic_Intel "-ftrapuv")
  set(Fortran_TRAPUV_Windows_Intel "/Qtrapuv")

  set(Fortran_CHECKALL_Generic_GNU "-fcheck=all")
  set(Fortran_CHECKALL_Generic_Intel "-check all")
  set(Fortran_CHECKALL_Windows_Intel "/check:all")

  set(Fortran_CHECKNONE_Generic_GNU "-fcheck=no-all")
  set(Fortran_CHECKNONE_Generic_Intel "-check none")
  set(Fortran_CHECKNONE_Windows_Intel "/nocheck")

  set(Fortran_DBGMIN_Generic_GNU "")
  set(Fortran_DBGMIN_Generic_Intel "-debug minimal")
  set(Fortran_DBGMIN_Windows_Intel "/debug:minimal")

  set(Fortran_CHECKNOARGTMP_Generic_GNU "-fcheck=no-array-temps")
  set(Fortran_CHECKNOARGTMP_Generic_Intel "-check noarg_temp_created")
  set(Fortran_CHECKNOARGTMP_Windows_Intel "/check:noarg_temp_created")

  set(Fortran_CHECKBASIC_Generic_GNU "-fcheck=pointer,bounds")
  set(Fortran_CHECKBASIC_Generic_Intel "-check pointer,bounds,uninit,format,output_conversion")
  set(Fortran_CHECKBASIC_Windows_Intel "/check:pointer,bounds,uninit,format,output_conversion")

  set(Fortran_THREADS_Generic_GNU "-pthread")
  set(Fortran_THREADS_Generic_Intel "-threads")
  set(Fortran_THREADS_Windows_Intel "/threads")

  set(Fortran_NOOMFP_Generic_GNU "-fno-omit-frame-pointer")
  set(Fortran_NOOMFP_Generic_Intel "-fno-omit-frame-pointer")
  set(Fortran_NOOMFP_Windows_Intel "/Oy-")

  set(Fortran_STACK_Generic_GNU "")
  set(Fortran_STACK_Generic_Intel "")
  set(Fortran_STACK_Windows_Intel "/STACK:10000000,10000000")

  set(Fortran_PROF_Generic_GNU "-pg")
  set(Fortran_PROF_Generic_Intel "-pg")
  set(Fortran_PROF_Windows_Intel "")

  set(Fortran_NOFTZ_Generic_GNU "")
  set(Fortran_NOFTZ_Generic_Intel "-no-ftz")
  set(Fortran_NOFTZ_Windows_Intel "/Qftz-")

  set(Fortran_SAVE_Generic_GNU "-fno-automatic")
  set(Fortran_SAVE_Generic_Intel "-save")
  set(Fortran_SAVE_Windows_Intel "/Qsave")

  set(Fortran_STDF90_Generic_GNU "")
  set(Fortran_STDF90_Generic_Intel "-stand f90")
  set(Fortran_STDF90_Windows_Intel "/stand:f90")

  set(Fortran_STDF95_Generic_GNU "-std f95")
  set(Fortran_STDF95_Generic_Intel "-stand f95")
  set(Fortran_STDF95_Windows_Intel "/stand:f95")

  set(Fortran_STDF03_Generic_GNU "-std f2003")
  set(Fortran_STDF03_Generic_Intel "-stand f03")
  set(Fortran_STDF03_Windows_Intel "/stand:f03")

  set(Fortran_VSMP_Generic_GNU "")
  set(Fortran_VSMP_Generic_Intel "")
  set(Fortran_VSMP_Windows_Intel "/MP")

  if(NOT MSVC)
    set(CMAKE_SYSTEM_NAME Generic)
  endif()

  if(NOT DEFINED Fortran_${name}_${CMAKE_SYSTEM_NAME}_${CMAKE_Fortran_COMPILER_ID})
    message(AUTHOR_WARNING "Fortran flag ${name} not found. "
      "If typing is correct, the root project's cmi.cmake file needs to be updated."
    )
  endif()

  set(NEW_FLAG "${Fortran_${name}_${CMAKE_SYSTEM_NAME}_${CMAKE_Fortran_COMPILER_ID}}")
  if(NOT ${var} MATCHES "${NEW_FLAG}")
    string(STRIP "${${var}} ${NEW_FLAG}" ${var})
    set(${var} "${${var}}" PARENT_SCOPE)
  endif()

  if(${name} STREQUAL "FPP")
    message(DEPRECATION "Fortran FPP flag has been deprecated and can be replaced by CPP and EXTENDEDLINESF90. "
    "Also consider using uppercase extension .F90, which will implicitly activate preprocessing.")
  endif()

endfunction()

function(cmi_fortran_default_mangling)
  set(flag "/names:lowercase /assume:underscore")
  if(MSVC)
    string(FIND "${CMAKE_Fortran_FLAGS}" "${flag}" status)
    if("${status}" STREQUAL "-1")
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${flag}" PARENT_SCOPE)
    endif()
  endif()
endfunction()

macro(cmi_disable_vs_manifest_ TARGET_FLAG_)
  if(MSVC)
    set(flag "/MANIFEST:NO")
    if(DEFINED ${TARGET_FLAG_})
      string(FIND "${${TARGET_FLAG_}}" "${flag}" status)
      if("${status}" STREQUAL "-1")
        set(${TARGET_FLAG_} "${${TARGET_FLAG_}} ${flag}" CACHE STRING "" FORCE)
      endif()
    endif()
  endif()
endmacro()

function(cmi_disable_vs_manifest)
    cmi_disable_vs_manifest_(CMAKE_EXE_LINKER_FLAGS)
    cmi_disable_vs_manifest_(CMAKE_SHARED_LINKER_FLAGS)
    cmi_disable_vs_manifest_(CMAKE_MODULE_LINKER_FLAGS)
endfunction()

macro(cmi_enable_vs_z7_ FLAG_)
  if(DEFINED ${FLAG_})
    # Replace /Zi and /ZI by /Z7
    string(REGEX REPLACE "/Z[iI]" "/Z7" ${FLAG_} "${${FLAG_}}")

    # Explicitly add /Z7 if /debug:full or /debug:minimal is set
    string(FIND "${${FLAG_}}" "/Z7" status)
    if("${status}" STREQUAL "-1")
      string(REGEX REPLACE "/debug:full" "/debug:full /Z7" ${FLAG_} "${${FLAG_}}")
      string(REGEX REPLACE "/debug:minimal" "/debug:minimal /Z7" ${FLAG_} "${${FLAG_}}")
    endif()

    string(STRIP "${${FLAG_}}" ${FLAG_})
    set(${FLAG_} "${${FLAG_}}" CACHE STRING "" FORCE)
  endif()
endmacro()

function(cmi_enable_vs_z7)
  if(MSVC)
    set(LANGUAGES "C" "CXX" "Fortran")
    foreach(LANGUAGE IN LISTS LANGUAGES)
      foreach(CONFIGURATION IN LISTS CMAKE_CONFIGURATION_TYPES)
        string(TOUPPER ${CONFIGURATION} CONFIGURATION)
        cmi_enable_vs_z7_(CMAKE_${LANGUAGE}_FLAGS_${CONFIGURATION})
      endforeach()
      cmi_enable_vs_z7_(CMAKE_${LANGUAGE}_FLAGS)
    endforeach()
  endif()
endfunction()


macro(cmi_disable_vs_incremental_linker_ FLAG_)
  if(DEFINED ${FLAG_})
    string(REGEX REPLACE "/INCREMENTAL(:YES)?($| )" "/INCREMENTAL:NO" ${FLAG_} "${${FLAG_}}")
    string(STRIP "${${FLAG_}}" ${FLAG_})
    set(${FLAG_} "${${FLAG_}}" CACHE STRING "" FORCE)
  endif()
endmacro()

function(cmi_disable_vs_incremental_linker)
  if(MSVC)
    foreach(CONFIGURATION IN LISTS CMAKE_CONFIGURATION_TYPES)
      string(TOUPPER ${CONFIGURATION} CONFIGURATION)
      cmi_disable_vs_incremental_linker_(CMAKE_EXE_LINKER_FLAGS_${CONFIGURATION})
      cmi_disable_vs_incremental_linker_(CMAKE_SHARED_LINKER_FLAGS_${CONFIGURATION})
      cmi_disable_vs_incremental_linker_(CMAKE_STATIC_LINKER_FLAGS_${CONFIGURATION})
      cmi_disable_vs_incremental_linker_(CMAKE_MODULE_LINKER_FLAGS_${CONFIGURATION})
    endforeach()
  endif()
endfunction()

function(cmi_disable_vs_debug_runtime)
  if(MSVC)
    set(LANGUAGES "C" "CXX" "Fortran")
    foreach(LANGUAGE IN LISTS LANGUAGES)
      set(TARGET_FLAG "CMAKE_${LANGUAGE}_FLAGS_DEBUG")
      if(DEFINED ${TARGET_FLAG})
        set(FLAGS "${${TARGET_FLAG}}")
        string(REPLACE "/dbglibs " " " FLAGS "${FLAGS} ")
        string(REPLACE "/MDd " "/MD " FLAGS "${FLAGS} ")
        string(REPLACE "/MTd " "/MT " FLAGS "${FLAGS} ")
        string(STRIP "${FLAGS}" FLAGS)
        set(${TARGET_FLAG} "${FLAGS}" CACHE STRING "" FORCE)
      endif()
    endforeach()
  endif()
endfunction()

function(cmi_enable_vs_mp)
  if(MSVC)
    cmi_Fortran_append(flag VSMP)
    set(LANGUAGES "C" "CXX" "Fortran")
    foreach(LANGUAGE IN LISTS LANGUAGES)
      set(TARGET_FLAG "CMAKE_${LANGUAGE}_FLAGS")
      if(DEFINED ${TARGET_FLAG})
        if(${CMAKE_VERSION} VERSION_LESS "3.19.0" AND LANGUAGE STREQUAL "Fortran")
          # Skip /MP as older CMake versions can't set /Z7 to prevent corrupt PDB files
          message(STATUS "CMake version 3.19.0 is required to enable parallel building of Fortran projects. Skipped.")
          continue()
        endif()
        string(FIND "${${TARGET_FLAG}}" "${flag}" status)
        if("${status}" STREQUAL "-1")
          string(STRIP "${${TARGET_FLAG}}" ${TARGET_FLAG})
          set(${TARGET_FLAG} "${${TARGET_FLAG}} ${flag}" CACHE STRING "" FORCE)
        endif()
      endif()
    endforeach()
  endif()
endfunction()

function(cmi_disable_vs_mp)
  if(MSVC)
    cmi_Fortran_append(flag VSMP)
    set(LANGUAGES "C" "CXX" "Fortran")
    foreach(LANGUAGE IN LISTS LANGUAGES)
      set(TARGET_FLAG "CMAKE_${LANGUAGE}_FLAGS")
      if(DEFINED ${TARGET_FLAG})
        string(REGEX REPLACE "${flag}" "" ${TARGET_FLAG} "${${TARGET_FLAG}}")
        string(STRIP "${${TARGET_FLAG}}" ${TARGET_FLAG})
        set(${TARGET_FLAG} "${${TARGET_FLAG}}" CACHE STRING "" FORCE)
      endif()
    endforeach()
  endif()
endfunction()

######################################
# BUILD SETTINGS
######################################

# Set default path variables
macro(cmi_load_build_environment)
  message(DEPRECATION "cmi_load_build_environment() deprecated. Use cmi_set_build_environment() instead.")
  cmi_set_build_environment(${ARGV})
endmacro()
macro(cmi_set_build_environment)
  if(POLICY CMP0068)
    cmake_policy(SET CMP0068 NEW)
  endif()

  set(CMI_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/package/$<CONFIG>" CACHE PATH "")
  set(CMI_BINARAY_OUTPUT_DIRECTORY "${CMI_OUTPUT_DIRECTORY}/bin" CACHE PATH "")
  mark_as_advanced(CMI_OUTPUT_DIRECTORY)
  mark_as_advanced(CMI_BINARAY_OUTPUT_DIRECTORY)

  # Predefined target properties
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMI_BINARAY_OUTPUT_DIRECTORY}")
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMI_BINARAY_OUTPUT_DIRECTORY}")
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMI_BINARAY_OUTPUT_DIRECTORY}")
  set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
  if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set(CMAKE_INSTALL_RPATH "@loader_path")
  else()
    set(CMAKE_INSTALL_RPATH "$ORIGIN")
  endif()
  set(CMAKE_FOLDER "${PROJECT_NAME}")


  # Enabled testing by default
  option(BUILD_TESTING "" OFF)
  if(BUILD_TESTING)
    enable_testing()
  endif()

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

  if(MSVC)
    option(BUILD_PARALLEL "Enable parallel builds. To switch off, deleting cache is required! Should be combined with BUILD_VS_Z7" OFF)
    if(BUILD_PARALLEL)
      cmi_enable_vs_mp()
    else()
      cmi_disable_vs_mp()
    endif()

    option(BUILD_VS_Z7 "Avoid corrupted pdb files when compiling in parallel. To switch off, deleting cache is required!" ${BUILD_PARALLEL})
    if(BUILD_VS_Z7)
      cmi_enable_vs_z7()
    endif()

    option(BUILD_NO_MANIFEST "Disable manifest generation. To switch off, deleting cache is required!" ON)
    if(BUILD_NO_MANIFEST)
      cmi_disable_vs_manifest()
    endif()
    mark_as_advanced(BUILD_PARALLEL BUILD_NO_MANIFEST BUILD_VS_Z7)
  endif()

  cmi_disable_vs_incremental_linker()
  cmi_disable_vs_debug_runtime()
  if(CMAKE_Fortran_COMPILER)
    cmi_fortran_default_mangling()
    cmi_fortran_append(CMAKE_Fortran_FLAGS_DEBUG O0)
    #cmi_fortran_append(CMAKE_Fortran_FLAGS TRACEBACK)
  endif()
endmacro()

function(cmi_set_directory TARGET_)
  set(OPTIONS_ "OUTPUT" "IDE")
  cmake_parse_arguments("" "" "${OPTIONS_}" "" ${ARGN})

  if(NOT TARGET ${TARGET_})
    message(WARNING "Can not set directoy for unknown target ${TARGET_}")
    return()
  endif()

  if(DEFINED _OUTPUT)
    if(NOT IS_ABSOLUTE "${_OUTPUT}")
      set(_OUTPUT "${CMI_BINARAY_OUTPUT_DIRECTORY}/${_OUTPUT}")
      #get_property(_OUTPUT TARGET ${TARGET_} PROPERTY RUNTIME_OUTPUT_DIRECTORY)
    endif()
    file(RELATIVE_PATH PATH_TO_BIN "${_OUTPUT}" "${CMI_BINARAY_OUTPUT_DIRECTORY}")
    set_property(TARGET ${TARGET_} PROPERTY ARCHIVE_OUTPUT_DIRECTORY "${_OUTPUT}")
    set_property(TARGET ${TARGET_} PROPERTY LIBRARY_OUTPUT_DIRECTORY "${_OUTPUT}")
    set_property(TARGET ${TARGET_} PROPERTY RUNTIME_OUTPUT_DIRECTORY "${_OUTPUT}")
    set_property(TARGET ${TARGET_} PROPERTY BUILD_WITH_INSTALL_RPATH TRUE)
    if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
      set_property(TARGET ${TARGET_} APPEND PROPERTY INSTALL_RPATH "@loader_path/${PATH_TO_BIN}")
    else()
      set_property(TARGET ${TARGET_} APPEND PROPERTY INSTALL_RPATH "$ORIGIN/${PATH_TO_BIN}")
    endif()
  endif()

  if(DEFINED _IDE)
    string(REGEX REPLACE "^/" "" _IDE_NEW ${_IDE})
    if(_IDE_NEW STREQUAL _IDE)
      set_property(TARGET ${TARGET_} PROPERTY FOLDER "${PROJECT_NAME}/${_IDE}")
    else()
      set_property(TARGET ${TARGET_} PROPERTY FOLDER "${_IDE_NEW}")
    endif()
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

function(cmi_fortran_check_newline_style)
  set(_INFO_MESSAGE "Detecting Fortran newline style")
  if(DEFINED HAVE_FORTRAN_NEWLINE_STYLE_CRLF AND DEFINED HAVE_FORTRAN_NEWLINE_STYLE_LF)
    return()
  endif()
  message(STATUS "${_INFO_MESSAGE}")

  # Compile and run test program to detect Fortran newline style
  set(CHECK_LINE_ENDINGS
    "program check_line_endings
  implicit none
  integer :: i
  integer :: file_size
  character, dimension(:), allocatable :: data
  character(len=*), parameter :: file_name = 'check_newline_style.txt'
  open(20, file=file_name, form='FORMATTED')
  write(20,'(A)') ''
  close(20)
  open(20, file=file_name, access='STREAM', form='UNFORMATTED')
  inquire(file=file_name, size=file_size)
  allocate(data(file_size))
!  write(*,*) 'size', file_size
  do i = 1, file_size
    read(20) data(i)
!    write(*,'(I3)') ichar(data(i))
  end do
  close(20, status='delete')
  if (file_size .eq. 2 .and. data(1) .eq. char(13) .and. data(2) .eq. char(10)) then
    write(*,'(A)') 'CRLF'
  else if (file_size .eq. 1 .and. data(1) .eq. char(10)) then
    write(*,'(A)') 'LF'
  else
    write(*,'(A)') 'UNKNOWN'
  end if
end program"
  )
  file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/check_newline_style.F90 ${CHECK_LINE_ENDINGS})
  try_run(RUN_RESULT_VAR COMPILE_RESULT_VAR ${CMAKE_CURRENT_BINARY_DIR} SOURCES ${CMAKE_CURRENT_BINARY_DIR}/check_newline_style.F90 RUN_OUTPUT_VARIABLE output)
  file(REMOVE ${CMAKE_CURRENT_BINARY_DIR}/check_newline_style.F90)
  # Something went wrong, set manually
  if(NOT COMPILE_RESULT_VAR OR (NOT RUN_RESULT_VAR STREQUAL 0))
    if(WIN32)
      set(output "CRLF")
    else()
      set(output "LF")
    endif()
    message(STATUS "${_INFO_MESSAGE} - failed, using fallback")
  endif()
  string(STRIP "${output}" output)
  message(STATUS "${_INFO_MESSAGE} - '${output}'")
  if(output STREQUAL "CRLF")
    set(HAVE_FORTRAN_NEWLINE_STYLE_CRLF 1 CACHE STRING "")
  else()
    set(HAVE_FORTRAN_NEWLINE_STYLE_CRLF 0 CACHE STRING "")
  endif()
  mark_as_advanced(HAVE_FORTRAN_NEWLINE_STYLE_CRLF)
  if(output STREQUAL "LF")
    set(HAVE_FORTRAN_NEWLINE_STYLE_LF 1 CACHE STRING "")
  else()
    set(HAVE_FORTRAN_NEWLINE_STYLE_LF 0 CACHE STRING "")
  endif()
  mark_as_advanced(HAVE_FORTRAN_NEWLINE_STYLE_LF)
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
  add_library(${LIB_NAME_} INTERFACE)
  target_link_libraries(${LIB_NAME_} INTERFACE "${LIB_DESTINATION_}/${LIB_SOURCE_NAME}")
  add_dependencies(${LIB_NAME_} ${LIB_NAME_}_import)
  cmi_set_directory(${LIB_NAME_}_import IDE "imports")
endfunction()

macro(cmi_include HEADER)
  if("${HEADER}" STREQUAL "GenerateExportHeader")
    # Generate export header
    if(${CMAKE_VERSION} VERSION_LESS "3.12.0" AND NOT CMAKE_CXX_COMPILER)
      message(STATUS "Applying GenerateExportHeader workaround for CMAKE < 3.12 - Enabling CXX")
      message(STATUS "See: https://gitlab.kitware.com/cmake/cmake/merge_requests/1799")
      check_language(CXX)
      if(CMAKE_CXX_COMPILER)
        enable_language(CXX)
      else()
        message(FATAL_ERROR "No CXX support")
      endif()
    endif()
  endif()
  include(${HEADER})
endmacro()

######################################
# FIND
######################################
# MPI
# Prequisites:
# If I_MPI_ROOT environment variable is set correctly, Intel MPI is used as library.
# Otherwise, passing a working MPI compiler wrapper is required.
# E.g. FC=mpif90 CC=mpicc CXX=mpicxx
#
# Example usage:
# cmi_find_mpi(REQUIRED COMPONENTS CXX)
# target_link_libraries(your_target PUBLIC CMI::MPI_CXX)
#
#| component | compiler | binding          | target          |
#|-----------|----------|------------------|-----------------|
#| F77       | Fortran  | MPI F77 mpif.h   | CMI::MPI_F77    |
#| F90       | Fortran  | MPI F90 mpi mod  | CMI::MPI_F90    |
#| C         | C        | MPI C            | CMI::MPI_C      |
#| CXX       | C++      | MPI C            | CMI::MPI_CXX    |
#| MPICXX    | C++      | MPI C++ (depcr.) | CMI::MPI_MPICXX |
#
#
macro(cmi_mpi_compiles_ COMPONENT RESULT)

  set(MPI_C_TEST_CODE_
    "#include<mpi.h>
      int main(int argc, char **args) {
        MPI_Init(&argc, &args);
        MPI_Finalize();
        return 0;
    }"
  )
  set(MPI_CXX_TEST_CODE_
    "#include<mpi.h>
      using namespace MPI;
      int main(int argc, char **args) {
        MPI_Init(&argc, &args);
        Info *info = new Info();
        MPI_Finalize();
        return 0;
    }"
  )
  set(MPI_F77_TEST_CODE_
    "program main
     implicit none
     include \"mpif.h\"
     integer :: ierror
     call MPI_Init(ierror)
     call MPI_Finalize(ierror)
     end program"
  )
  set(MPI_F90_TEST_CODE_
    "program main
    use mpi
    implicit none
    integer :: ierror
    call MPI_Init(ierror)
    call MPI_Finalize(ierror)
    end program"
  )

  if(${COMPONENT} STREQUAL "C")
    enable_language(C)
    include(CheckCCompilerFlag)
    CHECK_C_SOURCE_COMPILES("${MPI_C_TEST_CODE_}" ${RESULT})

  elseif(${COMPONENT} STREQUAL "CXX")
    enable_language(CXX)
    include(CheckCXXCompilerFlag)
    CHECK_CXX_SOURCE_COMPILES("${MPI_C_TEST_CODE_}" ${RESULT})

  elseif(${COMPONENT} STREQUAL "MPICXX")
    enable_language(CXX)
    include(CheckCXXCompilerFlag)
    CHECK_CXX_SOURCE_COMPILES("${MPI_CXX_TEST_CODE_}" ${RESULT})

  elseif(${COMPONENT} STREQUAL "F77" OR ${COMPONENT} STREQUAL "F90")
    enable_language(Fortran)
    include(CheckFortranCompilerFlag)
    if(${COMPONENT} STREQUAL "F77")
      CHECK_Fortran_SOURCE_COMPILES("${MPI_F77_TEST_CODE_}" ${RESULT} SRC_EXT F90)
    elseif(${COMPONENT} STREQUAL "F90")
      CHECK_Fortran_SOURCE_COMPILES("${MPI_F90_TEST_CODE_}" ${RESULT} SRC_EXT F90)
    endif()

  else()
    set(${RESULT} 0)
  endif()
endmacro()


# Setting up MPI
function(cmi_find_mpi)
  set(OPTIONS_ COMPONENTS)
  cmake_parse_arguments("" "REQUIRED" "" "${OPTIONS_}" ${ARGN})
  if(NOT DEFINED _COMPONENTS)
    if(CMAKE_C_COMPILER)
      list(APPEND _COMPONENTS C)
    endif()
    if(CMAKE_CXX_COMPILER)
      list(APPEND _COMPONENTS CXX MPICXX)
    endif()
    if(CMAKE_Fortran_COMPILER)
      list(APPEND _COMPONENTS F77 F90)
    endif()
  endif()

  # Intel MPI
  if(NOT I_MPI_ROOT)
    set(I_MPI_ROOT "$ENV{I_MPI_ROOT}" CACHE PATH "")
  endif()
  if(I_MPI_ROOT AND CYGWIN AND NOT DEFINED CMI_MPI_TYPE)
    message(STATUS "MPI: I_MPI_ROOT is set, but Intel MPI is not compatible with Cygwin!")
  endif()

  if(I_MPI_ROOT AND NOT CYGWIN)
    set(MPI_TYPE "Intel")
    if(NOT CMI_MPI_TYPE STREQUAL MPI_TYPE)
      message(STATUS "MPI: Using Intel MPI")
    endif()

    # (Re)initialize MPI
    if(NOT CMI_MPI_ROOT STREQUAL I_MPI_ROOT)
      message(STATUS "MPI: I_MPI_ROOT - ${I_MPI_ROOT}")
      foreach(COMPONENT IN ITEMS _COMPONENTS)
        unset(MPI_$[COMPONENT}_COMPILES CACHE)
      endforeach()
      unset(I_MPI_C_LIB_ CACHE)
      unset(I_MPI_CXX_LIB_ CACHE)
      unset(I_MPI_F_LIB_ CACHE)
    endif()

    set(MPI_ROOT "${I_MPI_ROOT}")

    # Handle MPI_EXEC
    set(MPI_EXEC "${I_MPI_ROOT}/intel64/bin/mpiexec")

    # Handle MPI_<comp>_LIB
    foreach(COMPONENT IN LISTS _COMPONENTS)
      set(I_MPI_LIB_DIR_ "${I_MPI_ROOT}/intel64/lib")

      # all require the C library
      find_library(I_MPI_C_LIB_ NAMES impi mpi PATHS "${I_MPI_LIB_DIR_}/release" NO_DEFAULT_PATH)
      set(MPI_${COMPONENT}_LIB ${I_MPI_C_LIB_})

      if(${COMPONENT} STREQUAL "MPICXX")
        find_library(I_MPI_CXX_LIB_ NAMES impicxx mpicxx PATHS "${I_MPI_LIB_DIR_}" NO_DEFAULT_PATH)
        if(I_MPI_CXX_LIB_)
          set(MPI_${COMPONENT}_LIB ${I_MPI_CXX_LIB_} ${I_MPI_C_LIB_})
        endif()

      elseif(${COMPONENT} STREQUAL "F77" OR ${COMPONENT} STREQUAL "F90")
        find_library(I_MPI_F_LIB_ NAMES mpifort PATHS "${I_MPI_LIB_DIR_}" NO_DEFAULT_PATH)
        if(I_MPI_F_LIB_)
          set(MPI_${COMPONENT}_LIB ${I_MPI_F_LIB_} ${I_MPI_C_LIB_})
        endif()

      endif()

      if(NOT MPI_${COMPONENT}_LIB)
        message(WARNING "MPI: Missing developer libraries (${COMPONENT}). MPI SDK installed correctly?")
      endif()
    endforeach()
    unset(I_MPI_LIB_DIR_)

    # Handle MPI_INCLUDE
    set(MPI_INCLUDE "${I_MPI_ROOT}/intel64/include")
    if(NOT EXISTS "${MPI_INCLUDE}")
      message(WARNING "MPI: Missing include directoy - ${MPI_INCLUDE}")
    else()
      if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        file(GLOB versions RELATIVE "${MPI_INCLUDE}/gfortran/" "${MPI_INCLUDE}/gfortran/*")
        set(TARGET_VERSION "${CMAKE_Fortran_COMPILER_VERSION}")
        foreach(version IN LISTS versions)
          if(NOT version VERSION_GREATER TARGET_VERSION)
            if(version VERSION_GREATER CURRENT_VERSION)
              set(CURRENT_VERSION ${version})
            endif()
          endif()
        endforeach()
        if(CURRENT_VERSION)
          list(INSERT MPI_INCLUDE 0 "${MPI_INCLUDE}/gfortran/${CURRENT_VERSION}")
        endif()
      endif()
    endif()
    # /Intel MPI

  else()
     # Wrapper MPI
    set(MPI_TYPE "Wrapper")
    if(NOT CMI_MPI_TYPE STREQUAL MPI_TYPE)
      message(STATUS "MPI: Using compiler wrapper")
    endif()
    set(MPI_EXEC "mpiexec")
    # /Wrapper MPI
  endif()


  # Check if MPI components work
  foreach(COMPONENT IN LISTS _COMPONENTS)
    if(NOT TARGET CMI_MPI_${COMPONENT})
      set(CMAKE_REQUIRED_INCLUDES ${MPI_INCLUDE})
      set(CMAKE_REQUIRED_LIBRARIES ${MPI_${COMPONENT}_LIB})
      cmi_mpi_compiles_(${COMPONENT} MPI_${COMPONENT}_COMPILES)
      unset(CMAKE_REQUIRED_INCLUDES)
      unset(CMAKE_REQUIRED_LIBRARIES)
      if(MPI_${COMPONENT}_COMPILES)
        add_library(CMI_MPI_${COMPONENT} INTERFACE)
        set_property(TARGET CMI_MPI_${COMPONENT} PROPERTY INTERFACE_INCLUDE_DIRECTORIES "${MPI_INCLUDE}")
        set_property(TARGET CMI_MPI_${COMPONENT} PROPERTY INTERFACE_LINK_LIBRARIES "${MPI_${COMPONENT}_LIB}")
        add_library(CMI::MPI_${COMPONENT} ALIAS CMI_MPI_${COMPONENT})
      else()
        unset(MPI_${COMPONENT}_COMPILES CACHE)
        list(APPEND CMI_MPI_MISSING ${COMPONENT})
      endif()
    endif()
  endforeach()

  set(CMI_MPI_ROOT "${MPI_ROOT}" CACHE INTERNAL "")
  set(CMI_MPI_TYPE "${MPI_TYPE}" CACHE INTERNAL "")

  # Set mpiexec from environment
  if(NOT MPI_EXEC OR NOT EXISTS "${MPI_EXEC}")
    set(MPI_EXEC "$ENV{MPIEXEC}")
  endif()
  # Set mpiexec to default
  if(NOT MPI_EXEC OR NOT EXISTS "${MPI_EXEC}")
    set(MPI_EXEC "mpiexec")
  endif()
  set(CMI_MPIEXEC "${MPI_EXEC}" CACHE INTERNAL "")

  if(CMI_MPI_MISSING)
    message(STATUS "MPI: Unable to use the following component(s): ${CMI_MPI_MISSING}")
    if(_REQUIRED)
      message(SEND_ERROR "MPI: Unsatisfied requirements.")
    endif()
  endif()

endfunction()


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
    set(Python_LIBRARIES "${PYTHON_LIBRARIES}")
    set(Python_LIBRARY_DIRS "${PYTHON_LIBRARY_DIRS}")
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
  cmi_Fortran_append(OMP_FLAGS_ OMP)
  if(NOT TARGET CMI_OpenMP_Fortran)
    add_library(CMI_OpenMP_Fortran INTERFACE)
    set_property(TARGET CMI_OpenMP_Fortran PROPERTY INTERFACE_COMPILE_OPTIONS ${OMP_FLAGS_})
    if(NOT MSVC)
      target_link_libraries(CMI_OpenMP_Fortran INTERFACE ${OMP_FLAGS_})
    endif()
    target_compile_definitions(CMI_OpenMP_Fortran INTERFACE _OMP)
    add_library(CMI::OpenMP_Fortran ALIAS CMI_OpenMP_Fortran)
  endif()
endfunction()
