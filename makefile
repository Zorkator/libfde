
TARGET_doc      := building libadt
TPP_FILES       := $(wildcard *.f90_tpp)
TPP_SOURCE      := $(patsubst %.f90_tpp,%.f90,$(TPP_FILES))
SOURCE_FILES    := $(filter-out test_%.f90,$(wildcard *.f90) $(TPP_SOURCE))
CLEARED_FILES   := $(TPP_SOURCE)
OUT_TYPE        := shared
OUT_NAME        := adt
OUT_DIR          = lib/$(mk_TAG)
FC_INCLUDE_DIRS := ./include

CONFIGURATIONS    = debug release
FC_CFLAGS.%       = $(fc_fpp) $(call fc_form,free,none)
FC_CFLAGS.debug   = $(fc_g)
FC_CFLAGS.release = $(fc_O3)

crc_impl.gfortran = -fno-range-check $(mk_FC_CFLAGS)
crc_impl.ifort    = -assume noold_boz $(mk_FC_CFLAGS)


%.f90: %.f90_tpp
	python typegen.py $< -o $@

map_test:
	@$(MAKE) built OUT_TYPE=static
	@$(MAKE) built OUT_TYPE=exe OUT_NAME=$@ SOURCE_FILES=test_hash_map.f90 FC_LIBRARIES=adt.$(mk_TAG) FC_INCLUDE_DIRS=$(OUT_DIR)

ifneq ($(MAKEIT_DIR),)
include $(MAKEIT_DIR)/mk.fortran
else
$(error environment variable MAKEIT_DIR not set! Please set it to home directory of makeIt)
endif

