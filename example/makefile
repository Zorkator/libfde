###
# makefile for building libfortres example
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

TARGET_doc      := building libfortres example
SOURCE_DIRS     := . ..
OUT_TYPE        := exe
OUT_NAME        := test_exception
OUT_DIR         := release

FC_INCLUDE_DIRS := ..
FC_FLAGS.%       = $(fc_omp)
FC_CFLAGS.%      = $(fc_fpp) $(call fc_form,free,none)
FC_PP_DEFINES.%  = _OMP
FC_LIBRARY_DIRS  = ../release
FC_LIBRARIES     = fortres.$(mk_CFG).$(mk_ARCH).gcc

ifneq ($(MAKEIT_DIR),)
include $(MAKEIT_DIR)/mk.fortran
else
$(error environment variable MAKEIT_DIR not set! Please set it to home directory of makeIt)
endif

