
F90C ?= gfortran
CFG  ?= debug

mk_F90_FLAGS_gfortran_debug   = -ggdb -cpp -ffree-line-length-none
mk_F90_FLAGS_gfortran_release = -O3 -cpp -ffree-line-length-none
mk_F90C_gfortran              = gfortran-4.8

mk_F90_FLAGS_ifort_debug      = -g -fpp -allow nofpp-comments 
mk_F90_FLAGS_ifort_release    = -O3 -fpp -allow nofpp-comments 
mk_F90C_ifort                 = ifort

mk_F90_FLAGS        = $(mk_F90_FLAGS_$(F90C)_$(CFG))
mk_F90C             = $(mk_F90C_$(F90C))
mk_INCLUDE_PATHLIST = -I. -I./include

.SUFFIXES: ._f90
.SECONDARY: gref_test.f90 gref_test._f90
.INTERMEDIATE: gref_test._f90

union: test_union.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $< -o $@.$(F90C).$(CFG)
		
dynstring: dynamic_string.f90
	$(mk_F90C) $(mk_F90_FLAGS) -DTEST $(mk_INCLUDE_PATHLIST) $< -o $@.$(F90C).$(CFG)

gref: gref_test.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c dynamic_string.f90
	$(mk_F90C) $(mk_F90_FLAGS) -DTEST $(mk_INCLUDE_PATHLIST) $< dynamic_string.o -o $@.$(F90C).$(CFG)

gref_test._f90: generic_ref.f90
	cp $< $@

clean:
	rm -f *.mod *.o *.debug *.release


%.f90: %._f90
	python refgen.py $< > $@

%.o: %.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c $< -o $@

