###
# makefile for building libfortres (C-core library of libfde)
#
# NOTE: for using this makefile you need some meta-makefiles bundled by another project called 'makeIt'.
#       Please set the environment variable MAKEIT_DIR to a directory that contains a copy.
#       For getting a copy you can do ...
#      		git clone https://github.com/Zorkator/makeIt makeIt
#       or
#         svn co https://github.com/Zorkator/makeIt/trunk makeIt
#
# Type 'make' to get the help screen, or 'make built' to start building libfortres.
#

TARGET_doc      := building libfortres
SOURCE_FILES    := fort_exception.cpp fort_plugin.cpp fort_dirent.cpp fort_tracestack.cpp
OUT_TYPE        := shared
OUT_NAME         = fortres.x$(mk_ARCH)
OUT_DIR          = release
MAYOR           := 2.7.1
CC_INCLUDE_DIRS := ./include ../include
CC_LIBRARIES     = dl
CC_FLAGS.%       = $(cc_threads) $(cc_m)$(mk_ARCH)

ifneq ($(MAKEIT_DIR),)
include $(MAKEIT_DIR)/mk.c++
else
$(error environment variable MAKEIT_DIR not set! Please set it to home directory of makeIt)
endif

