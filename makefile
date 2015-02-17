
TPP_FILES       = $(wildcard *.tpp)
ADT_MODULES     = crc memoryref typeinfo basestring ref string list basetypes item hashmap
IMPL_MODULES    = $(filter $(wildcard *_impl.f90),$(ADT_MODULES:%=%_impl.f90))
SOURCE_FILES    = $(ADT_MODULES:%=%.f90) $(IMPL_MODULES)
BUILT_TYPE      = shared
OUT_NAME        = adt.0
OUT_DIR         = exec
FC_INCLUDE_DIRS = ./include
SUB_PACKAGES    = submod

CONFIGURATIONS    = debug release #test_hashmap test_map
FC_CFLAGS.%       = $(fc_fpp) $(call fc_form,free,none)
FC_CFLAGS.debug   = $(fc_g)
FC_CFLAGS.release = $(fc_O3)

crc_impl.gfortran = -fno-range-check $(mk_FC_CFLAGS)
crc_impl.ifort    = -assume noold_boz $(mk_FC_CFLAGS)


#SOURCE_FILES.test_hashmap = $(SOURCE_FILES) test_hash_map.f90
#BUILT_TYPE.test_hashmap   = exe
#OUT_NAME.test_hashmap     = test_hashmap
#
#SOURCE_FILES.test_map    = test_hash_map.f90
#FC_INCLUDE_DIRS.test_map = $(OUT_DIR) $(FC_INCLUDE_DIRS)
#FC_LIBRARIES.test_map    = adt.0.debug.32.gfortran
#BUILT_TYPE.test_map      = exe
#OUT_NAME.test_map        = test_map


MAKEIT_DIR = ../makeIt/rework
include $(MAKEIT_DIR)/mk.compilation
include $(MAKEIT_DIR)/mk.fortran
sinclude $(mk_DEPS)

%.f90: %.f90.tpp
	python typegen.py $< -o $@


map_test:
	@$(MAKE) BUILT_TYPE=static
	@$(MAKE) BUILT_TYPE=exe SOURCE_FILES=test_hash_map.f90 FC_LIBRARIES=adt.0.$(mk_TAG) FC_INCLUDE_DIRS=$(OUT_DIR)

