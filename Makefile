
F90C ?= gfortran
CFG  ?= debug
ARCH ?= 64
MYOR ?= 0

mk_F90_FLAGS_gfortran_debug   = -ggdb -cpp -ffree-line-length-none $(_F90_FLAGS)
mk_F90_FLAGS_gfortran_release = -O3 -cpp -ffree-line-length-none $(_F90_FLAGS)
mk_F90C_gfortran              = gfortran #< need 4.9 or later, prior versions are just too buggy!

mk_F90_FLAGS_ifort_debug      = -g -fpp -allow nofpp-comments $(_F90_FLAGS)
mk_F90_FLAGS_ifort_release    = -O3 -fpp -allow nofpp-comments $(_F90_FLAGS)
mk_F90C_ifort                 = ifort

mk_F90C_PP_FLAGS    = $(PP_FLAGS:%=-D%)
mk_F90_FLAGS        = $(mk_F90_FLAGS_$(F90C)_$(CFG)) -m$(ARCH) $(mk_F90C_PP_FLAGS)
mk_F90C             = $(mk_F90C_$(F90C))
mk_INCLUDE_PATHLIST = -I. -I./include

mk_TAG              = $(MYOR).$(F90C).$(CFG).$(ARCH)

.SECONDARY:
.PHONY: clean

TPP_FILES = $(wildcard *.tpp)
BASE_OBJ  = crc.o crc_impl.o memoryref.o typeinfo.o typeinfo_impl.o basestring.o basestring_impl.o ref.o ref_impl.o string.o string_impl.o \
						list.o list_impl.o basetypes.o item.o item_impl.o hashmap.o hashmap_impl.o

# file specific compiler flags ...
crc_impl_F90_FLAGS_gfortran  = -fno-range-check
crc_impl_F90_FLAGS_ifort     = -assume noold_boz


all: clean dynstring gref item alist map

base: $(BASE_OBJ)

libadt: clean
	$(MAKE) _F90_FLAGS="-fpic -DBUILT_TYPE=SHARED_LIB" base
	$(mk_F90C) -shared -m$(ARCH) $(BASE_OBJ) -o $@.$(mk_TAG).so
	mkdir -p lib/$(F90C).$(ARCH)
	cp $@.$(mk_TAG).so adt_*.mod lib/$(F90C).$(ARCH)

libcall: libadt test_lib_call.o
	$(mk_F90C) test_lib_call.o -L. -ladt.$(mk_TAG) -lcrc -o $@.$(mk_TAG)

dynstring: $(BASE_OBJ) test_string.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

gref: $(BASE_OBJ) test_type_references.o test_ref.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

item: $(BASE_OBJ) test_item.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

alist: $(BASE_OBJ) test_abstract_list.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

map: $(BASE_OBJ) test_hash_map.o
	$(mk_F90C) $(mk_F90_FLAGS) $(mk_INCLUDE_PATHLIST) $? -o $@.$(mk_TAG)

clean:
	rm -f *.mod *.o *.$(mk_TAG) $(TPP_FILES:%.tpp=%)

test: clean dynstring gref item alist map
	./dynstring.$(mk_TAG)
	./gref.$(mk_TAG)
	./item.$(mk_TAG)
	./alist.$(mk_TAG)
	./map.$(mk_TAG)

testsim: libadt
	$(MAKE) _F90_FLAGS="-fpic" test_simulator.o
	$(mk_F90C) -ggdb -shared -m$(ARCH) -L. -ladt.$(mk_TAG) test_simulator.o -o test_simulator.$(mk_TAG).so
	ln -fs test_simulator.$(mk_TAG).so test_simulator.so

testsimdriver: testsim
	$(MAKE) test_simulator_driver.o
	$(mk_F90C) -ggdb -m$(ARCH) -L. test_simulator_driver.o -ladt.$(mk_TAG) test_simulator.$(mk_TAG).so -o test_simulator_driver.$(mk_TAG)
	ln -fs test_simulator_driver.$(mk_TAG) test_simulator_driver

%.f90: %.f90.tpp
	python typegen.py $< -o $@

%.o: %.f90
	$(mk_F90C) $(mk_F90_FLAGS) $($(notdir $*)_F90_FLAGS_$(F90C)) $(mk_INCLUDE_PATHLIST) -c $< -o $@

