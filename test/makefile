###
# makefile for building tests of libfde
#
# NOTE: for using this makefile you need some meta-makefiles bundled by another project called 'makeIt'.
#       Please set the environment variable MAKEIT_DIR to a directory that contains a copy.
#       For getting a copy you can do ...
#         git clone https://github.com/Zorkator/makeIt makeIt
#       or
#         svn co https://github.com/Zorkator/makeIt/trunk makeIt
#
# Type 'make' to get the help screen, or 'make built' to start building.
#

TARGET_doc      := building fde-tests
OUT_TYPE        := exe
OUT_NAME         = test.$(mk_TAG)
OUT_DIR          = exe/$(mk_TAG)
FDE_DIR          = ..
SOURCE_DIRS      = .
FC_INCLUDE_DIRS  = ../include $(FDE_DIR)/lib/$(mk_TAG)
src_list         = $1.f90 $(patsubst %.f90_tpp,%.f90,$(wildcard $1_types.f90_tpp))

FC_LIBRARY_DIRS  = $(FDE_DIR)/lib/$(mk_TAG)
FC_LIBRARIES.%   = fde.$(mk_TAG) fortres.x$(mk_ARCH)

FC_list          = ifort gfortran
FC_FLAGS.%       = $(fc_threads) $(fc_m)$(mk_ARCH)
FC_CFLAGS.%      = $(fc_fpp) $(call fc_form,free,none) $(fc_backtrace) $(call fc_fpe,0) $(fc_trapuv)
FC_LFLAGS.%      = -rdynamic #< add all symbols to dynamic symbol table (=> stacktrace)

TARGETS          = $(basename $(wildcard test_*.f90))

$(TARGETS):
	$(MAKE) SOURCE_FILES="$(call src_list,$@)" OUT_NAME=$@ built


%.f90: %.f90_tpp
	python ../typegen.py $< -o $@


ifneq ($(MAKEIT_DIR),)
include $(MAKEIT_DIR)/mk.fortran
else
$(error environment variable MAKEIT_DIR not set! Please set it to home directory of makeIt)
endif

