
TPP_FILES    = $(wildcard *.tpp)
ADT_MODULES  = crc memoryref typeinfo basestring ref string list basetypes item hashmap
IMPL_MODULES = $(filter $(wildcard *_impl.f90),$(ADT_MODULES:%=%_impl.f90))
SOURCE_FILES = $(ADT_MODULES:%=%.f90) $(IMPL_MODULES)
BUILT_TYPE   = shared
OUT_NAME     = adt.0
SUB_PACKAGES = submod

CONFIGURATIONS += special
SOURCE_FILES.special = $(SOURCE_FILES) test_hash_map.f90
BUILT_TYPE.special   = exe


FC_INCLUDE_DIRS   = ./include
FC_CFLAGS         = $(fc_fpp)
FC_CFLAGS.debug   = $(fc_g)
FC_CFLAGS.release = $(fc_O3)

crc_impl.gfortran = -fno-range-check
crc_impl.ifort    = -assume noold_boz

MAKEIT_DIR = ../makeIt/rework
include $(MAKEIT_DIR)/mk.compilation
include $(MAKEIT_DIR)/mk.fortran
sinclude $(mk_DEPS)

%.f90: %.f90.tpp
	python typegen.py $< -o $@

