
TPP_FILES       = $(wildcard *.f90_tpp)
SOURCE_FILES    = $(filter-out test_%.f90,$(wildcard *.f90)) $(TPP_FILES:%.f90_tpp:%.f90)
OUT_TYPE        = shared
OUT_NAME        = adt.0
OUT_DIR         = exec
FC_INCLUDE_DIRS = ./include

CONFIGURATIONS    = debug release
FC_CFLAGS.%       = $(fc_fpp) $(call fc_form,free,none)
FC_CFLAGS.debug   = $(fc_g)
FC_CFLAGS.release = $(fc_O3)

crc_impl.gfortran = -fno-range-check $(mk_FC_CFLAGS)
crc_impl.ifort    = -assume noold_boz $(mk_FC_CFLAGS)


MAKEIT_DIR = ../makeIt/rework
include $(MAKEIT_DIR)/mk.compilation
include $(MAKEIT_DIR)/mk.fortran

%.f90: %.f90_tpp
	python typegen.py $< -o $@


map_test:
	@$(MAKE) OUT_TYPE=static
	@$(MAKE) OUT_TYPE=exe OUT_NAME=$@ SOURCE_FILES=test_hash_map.f90 FC_LIBRARIES=adt.0.$(mk_TAG) FC_INCLUDE_DIRS=$(OUT_DIR)


$(mk_OUT_FILE): $(mk_OBJECTS)
	$(call mk_logged_cmd,$(cmd_LINK))
	$(cmd_FINALIZE)

$(mk_OBJECTS): | $(sort $(mk_OUTPUT_DIRS:%=%\/))


BINARY_TAG.exe = 'tag text                           [br]' \
                 'SVN Revision: $(svn_revision)      [br]' \
                 '$(if $(strip $(svn_status)),Modifications:[br] - $(call mk_join,[br] - ,$(svn_status)),)'

