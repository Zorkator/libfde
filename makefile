###
# makefile for building libadt
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

TARGET_doc      := building libadt
TPP_FILES       := $(filter-out test_%.f90_tpp,$(wildcard *.f90_tpp))
TPP_SOURCE      := $(patsubst %.f90_tpp,%.f90,$(TPP_FILES))
SOURCE_FILES    := $(filter-out test_%.f90 $(TPP_SOURCE),$(wildcard *.f90)) $(TPP_SOURCE)
CLEARED_FILES   := $(TPP_SOURCE)
OUT_TYPE        := shared
OUT_NAME         = adt.$(mk_TAG)
MAYOR           := 2
OUT_DIR          = lib/$(mk_TAG)
FC_INCLUDE_DIRS := ./include

FC_list = ifort gfortran

FC_CFLAGS.%       = $(fc_fpp) $(call fc_form,free,none) $(fc_backtrace)
crc_impl.gfortran = $(call fc_cflags_of,.f90) -fno-range-check
crc_impl.ifort    = $(call fc_cflags_of,.f90) -assume noold_boz

%.f90: %.f90_tpp
	python typegen.py $< -o $@


exe_opts = OUT_TYPE=exe OUT_DIR=exe OUT_NAME=$@.$(mk_TAG) \
           FC_INCLUDE_DIRS="include lib/$(mk_TAG)"
exe_libs = FC_LIBRARY_DIRS=lib/$(mk_TAG)				\
           FC_LIBRARIES=adt.$(mk_TAG)

ref_test:
	@$(MAKE) built OUT_NAME=adt.$(mk_TAG)
	@$(MAKE) built $(exe_opts) $(exe_libs)        \
	         SOURCE_FILES="test_ref.f90 test_type_references.f90"

test_adt:
	@$(MAKE) built OUT_NAME=adt.$(mk_TAG)
	@$(MAKE) built $(exe_opts) $(exe_libs)        \
	         SOURCE_FILES=test_adt.f90

map_test:
	@$(MAKE) built OUT_NAME=adt.$(mk_TAG)
	@$(MAKE) built $(exe_opts) $(exe_libs)        \
	         SOURCE_FILES=test_hash_map.f90

testsim:
	@$(MAKE) built OUT_NAME=adt.$(mk_TAG)
	@$(MAKE) built $(exe_opts) $(exe_libs) \
	         SOURCE_FILES=test_simulator.f90

testsim_so:
	@$(MAKE) built OUT_NAME=adt.$(mk_TAG)
	@$(MAKE) built $(exe_opts) $(exe_libs) OUT_TYPE=shared \
	         SOURCE_FILES=test_simulator.f90

testsimdriver: testsim_so
	@$(MAKE) built OUT_NAME=adt.$(mk_TAG)
	@$(MAKE) built $(exe_opts) FC_LIBRARY_DIRS="exe lib/$(mk_TAG)" FC_LIBRARIES="testsim_so.$(mk_TAG) adt.$(mk_TAG)"\
	         SOURCE_FILES=test_simulator_driver.f90				

all: ref_test test_adt map_test testsim testsimdriver

ifneq ($(MAKEIT_DIR),)
include $(MAKEIT_DIR)/mk.fortran
else
$(error environment variable MAKEIT_DIR not set! Please set it to home directory of makeIt)
endif

