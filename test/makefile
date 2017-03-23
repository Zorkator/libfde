###
# makefile for building tests of libadt
#
# NOTE: for using this makefile you need some meta-makefiles bundled by another project called 'makeIt'.
#       Please set the environment variable MAKEIT_DIR to a directory that contains a copy.
#       For getting a copy you can do ...
#         svn co htpps://subversion.assembla.com/svn/zappralot/trunk/makeIt
#
# Type 'make' to get the help screen, or 'make built' to start building.
#

TARGET_doc      := building adt-tests
OUT_TYPE        := exe
OUT_NAME         = test.$(mk_TAG)
OUT_DIR          = exe/$(mk_TAG)
ADT_DIR					 = ..
FC_INCLUDE_DIRS  = ../include $(ADT_DIR)/lib/$(mk_TAG)

#SUBPACKAGES.%    = ..
FC_LIBRARY_DIRS  = $(ADT_DIR)/lib/$(mk_TAG)
FC_LIBRARIES.%   = adt.$(mk_TAG) fortres.x$(mk_ARCH)

FC_list          = ifort gfortran
FC_FLAGS.%       = $(fc_threads) $(fc_m)$(mk_ARCH)
FC_CFLAGS.%      = $(fc_fpp) $(call fc_form,free,none) $(fc_backtrace) $(call fc_fpe,0) $(fc_trapuv)
FC_LFLAGS.%      = -rdynamic #< add all symbols to dynamic symbol table (=> stacktrace)

TARGETS          = $(basename $(wildcard test_*.f90))


$(TARGETS):
	$(MAKE) SOURCE_FILES=$@.f90 OUT_NAME=$@ built

#SOURCE_FILES     = test_adt.f90


#exe_opts = OUT_TYPE=exe OUT_DIR=exe OUT_NAME=$@.$(mk_TAG) \
#           FC_INCLUDE_DIRS="include lib/$(mk_TAG)"
#exe_libs = FC_LIBRARY_DIRS=lib/$(mk_TAG)				\
#           FC_LIBRARIES=adt.$(mk_TAG)
#
#ref_test:
#	@$(MAKE) built OUT_NAME=adt.$(mk_TAG)
#	@$(MAKE) built $(exe_opts) $(exe_libs)        \
#	         SOURCE_FILES="test_ref.f90 test_type_references.f90"
#
#test_adt:
#	@$(MAKE) built OUT_NAME=adt.$(mk_TAG)
#	@$(MAKE) built $(exe_opts) $(exe_libs)        \
#	         SOURCE_FILES=test_adt.f90
#
#map_test:
#	@$(MAKE) built OUT_NAME=adt.$(mk_TAG)
#	@$(MAKE) built $(exe_opts) $(exe_libs)        \
#	         SOURCE_FILES=test_hash_map.f90
#
#testsim:
#	@$(MAKE) built OUT_NAME=adt.$(mk_TAG)
#	@$(MAKE) built $(exe_opts) $(exe_libs) \
#	         SOURCE_FILES=test_simulator.f90
#
#testsim_so:
#	@$(MAKE) built OUT_NAME=adt.$(mk_TAG)
#	@$(MAKE) built $(exe_opts) $(exe_libs) OUT_TYPE=shared \
#	         SOURCE_FILES=test_simulator.f90
#
#testsimdriver: testsim_so
#	@$(MAKE) built OUT_NAME=adt.$(mk_TAG)
#	@$(MAKE) built $(exe_opts) FC_LIBRARY_DIRS="exe lib/$(mk_TAG)" FC_LIBRARIES="testsim_so.$(mk_TAG) adt.$(mk_TAG)"\
#	         SOURCE_FILES=test_simulator_driver.f90				
#
#all: ref_test test_adt map_test testsim testsimdriver

ifneq ($(MAKEIT_DIR),)
include $(MAKEIT_DIR)/mk.fortran
else
$(error environment variable MAKEIT_DIR not set! Please set it to home directory of makeIt)
endif

