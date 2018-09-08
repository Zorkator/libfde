###
# makefile for building libadt
#
# NOTE: for using this makefile you need some meta-makefiles bundled by another project called 'makeIt'.
#       Please set the environment variable MAKEIT_DIR to a directory that contains a copy.
#       For getting a copy you can do ...
#      		git clone https://github.com/Zorkator/makeIt makeIt
#       or
#         svn co https://github.com/Zorkator/makeIt/trunk makeIt
#
# Type 'make' to get the help screen, or 'make built' to start building libadt.
#

TARGET_doc      := building libadt
TPP_FILES       := $(filter-out test_%.f90_tpp,$(wildcard *.f90_tpp))
TPP_SOURCE      := $(patsubst %.f90_tpp,%.f90,$(TPP_FILES))
SOURCE_FILES    := $(filter-out test_%.f90 $(TPP_SOURCE),$(wildcard *.f90)) $(TPP_SOURCE)
CLEARED_FILES   := $(TPP_SOURCE)
OUT_TYPE        := shared
OUT_NAME         = adt.$(mk_TAG)
MAYOR           := 2.7.0
OUT_DIR          = lib/$(mk_TAG)
FC_INCLUDE_DIRS := ./libfortres/include ./include

SUBPACKAGES.%    = libfortres
FC_LIBRARIES.%   = fortres.x$(mk_ARCH)

FC_list = ifort gfortran

FC_FLAGS.%        = $(fc_threads) $(fc_m)$(mk_ARCH)
FC_CFLAGS.%       = $(fc_fpp) $(call fc_form,free,none) $(fc_backtrace)
crc_impl.gfortran = $(call fc_cflags_of,.f90) -fno-range-check
crc_impl.ifort    = $(call fc_cflags_of,.f90) -assume noold_boz

%.f90: %.f90_tpp
	python typegen.py $< -o $@

ifneq ($(MAKEIT_DIR),)
include $(MAKEIT_DIR)/mk.fortran
else
$(error environment variable MAKEIT_DIR not set! Please set it to home directory of makeIt)
endif

