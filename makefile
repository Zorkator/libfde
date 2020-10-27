
###
# makefile for building libfde
#
# NOTE: for using this makefile you need some meta-makefiles bundled by another project called 'makeIt'.
#       Please set the environment variable MAKEIT_DIR to a directory that contains a copy.
#       For getting a copy you can do ...
#      		git clone https://github.com/Zorkator/makeIt makeIt
#       or
#         svn co https://github.com/Zorkator/makeIt/trunk makeIt
#
# Type 'make' to get the help screen, or 'make built' to start building libfde.
#

TARGET_doc      := building libfde
TPP_FILES       := $(wildcard src/*.F90_tpp)
TPP_SOURCE      := $(patsubst %.F90_tpp,%.F90,$(TPP_FILES))
SOURCE_FILES    := $(filter-out $(TPP_SOURCE),$(wildcard src/*.F90)) $(TPP_SOURCE)
CLEARED_FILES   := $(TPP_SOURCE)
OUT_TYPE        := shared
OUT_NAME         = fde.$(mk_TAG)
MAYOR           := 2.8.0
OUT_DIR          = lib/$(mk_TAG)
FC_INCLUDE_DIRS := ./libfortres/include ./include

SUBPACKAGES.%    = libfortres
FC_LIBRARIES.%   = fortres.x$(mk_ARCH)

FC_FLAGS.%        = $(fc_threads) $(fc_m)$(mk_ARCH) $(fc_Wnointerfaces) #$(fc_Wall)
FC_CFLAGS.%       = $(fc_fpp) $(call fc_form,free,none) $(fc_backtrace)
FC_LFLAGS.%       = $(fl_dynamic)

crc_impl.gfortran = $(call fc_cflags_of,.F90) -fno-range-check
crc_impl.ifort    = $(call fc_cflags_of,.F90) -assume noold_boz

%.F90: %.F90_tpp
	python typegen.py $< -o $@

ifneq ($(MAKEIT_DIR),)
include $(MAKEIT_DIR)/mk.fortran
else
$(error environment variable MAKEIT_DIR not set! Please set it to home directory of makeIt)
endif

