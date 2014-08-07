
F90C ?= gfortran
CFG  ?= debug
ARCH ?= 64

mk_F90_FLAGS_gfortran_debug   = -ggdb -cpp -ffree-line-length-none
mk_F90_FLAGS_gfortran_release = -O3 -cpp -ffree-line-length-none
mk_F90C_gfortran              = gfortran

mk_F90_FLAGS_ifort_debug      = -g -fpp -allow nofpp-comments 
mk_F90_FLAGS_ifort_release    = -O3 -fpp -allow nofpp-comments 
mk_F90C_ifort                 = ifort

mk_F90_FLAGS        = $(mk_F90_FLAGS_$(F90C)_$(CFG)) -m$(ARCH)
mk_F90C             = $(mk_F90C_$(F90C))
mk_INCLUDE_PATHLIST = -I. -I./include

mk_TAG              = $(F90C).$(CFG).$(ARCH)

.SECONDARY:

TPP_FILES = $(wildcard *.tpp)
BASE_OBJ  = type_info.o base_string.o generic_ref.o base_types.o dynamic_string.o

all: clean dynstring gref varitem alist

base: $(BASE_OBJ)

dynstring: $(BASE_OBJ) test_dynamic_string.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

gref: $(BASE_OBJ) test_type_references.o test_generic_ref.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

varitem: $(BASE_OBJ) var_item.o test_var_item.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

glist: generic_list.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

alist: $(BASE_OBJ) var_item.o abstract_list.o test_abstract_list.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

clean:
	rm -f *.mod *.o *.debug.* *.release.* $(TPP_FILES:%.tpp=%)


%.f90: %.f90.tpp
	python typegen.py $< -o $@

%.o: %.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c $< -o $@

