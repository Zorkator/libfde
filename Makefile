
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


union: test_union.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $< -o $@.$(mk_TAG)
		
dynstring: dynamic_string.f90
	$(mk_F90C) $(mk_F90_FLAGS) -D TEST_DYNAMIC_STRING $(mk_INCLUDE_PATHLIST) $< -o $@.$(mk_TAG)

gref: test_generic_ref.f90 test_type_references.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c dynamic_string.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c generic_ref.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c test_type_references.f90
	$(mk_F90C) $(mk_F90_FLAGS) -D TEST_GENERIC_REF $(mk_INCLUDE_PATHLIST) $< dynamic_string.o -o $@.$(mk_TAG)

varitem: var_item.f90 gref_test.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c dynamic_string.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c gref_test.f90
	$(mk_F90C) $(mk_F90_FLAGS) -D TEST_VAR_ITEM $(mk_INCLUDE_PATHLIST) $< gref_test.o dynamic_string.o -o $@.$(mk_TAG)


glist: generic_list.f90 gref_test.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c dynamic_string.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c gref_test.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c var_item.f90
	$(mk_F90C) $(mk_F90_FLAGS) -D TEST_GENERIC_REF $(mk_INCLUDE_PATHLIST) $< var_item.o gref_test.o dynamic_string.o -o $@.$(F90C).$(CFG)


alist: abstract_list.f90 gref_test.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c dynamic_string.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c gref_test.f90
	$(mk_F90C) $(mk_F90_FLAGS) -D TEST_ABSTRACT_LIST $(mk_INCLUDE_PATHLIST) $< gref_test.o dynamic_string.o -o $@.$(F90C).$(CFG)


clean:
	rm -f *.mod *.o *.debug.* *.release.*


%.f90: %.f90.rg
	python refgen.py $< -o $@

%.o: %.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c $< -o $@

