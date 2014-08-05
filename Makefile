
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


all: clean dynstring gref varitem alist

bt: type_info.o dynamic_string.o generic_ref.o base_types.o

dynstring: base_string.o dynamic_string.o test_dynamic_string.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

gref: base_string.o dynamic_string.o type_info.o generic_ref.o test_type_references.o test_generic_ref.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

varitem: base_string.o dynamic_string.o type_info.o generic_ref.o test_type_references.o var_item.o test_var_item.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

glist: generic_list.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

alist: base_string.o dynamic_string.o type_info.o generic_ref.o test_type_references.o var_item.o abstract_list.o test_abstract_list.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

clean:
	rm -f *.mod *.o *.debug.* *.release.*


%.f90: %.f90.tpp
	python typegen.py $< -o $@

%.o: %.f90
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) -c $< -o $@

