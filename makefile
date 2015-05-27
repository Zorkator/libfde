###
# makefile for building libfortres
#
# NOTE: for using this makefile you need some meta-makefiles bundled by another project called 'makeIt'.
#       Please set the environment variable MAKEIT_DIR to a directory that contains a copy.
#       For getting a copy you can do ...
#         svn co htpps://subversion.assembla.com/svn/zappralot/trunk/makeIt
#
# You can test it without hassle by creating a static link to this file:
#   ln -s makefile.makeIt makefile
#
# Type 'make' to get the help screen, or 'make built' to start building libadt.
#

TARGET_doc      := building libfortres
SOURCE_FILES    := exception.cpp
OUT_TYPE        := shared
OUT_FILE_NAME    = libfortres.x$(mk_ARCH).so
OUT_DIR          = release
MAYOR           := 0
CC_INCLUDE_DIRS := .

ifneq ($(MAKEIT_DIR),)
include $(MAKEIT_DIR)/mk.c++
else
$(error environment variable MAKEIT_DIR not set! Please set it to home directory of makeIt)
endif

