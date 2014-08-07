#ifndef __VAR_ITEM_FPP
#define __VAR_ITEM_FPP

#include "adt/ppUtil.xpp"

# define _TableOf_varitem_types_base_ \
    _varitem_type_(bool,       logical)               \
    _varitem_type_(int8,       integer*1)             \
    _varitem_type_(int16,      integer*2)             \
    _varitem_type_(int32,      integer*4)             \
    _varitem_type_(int64,      integer*8)             \
    _varitem_type_(real32,     real*4)                \
    _varitem_type_(real64,     real*8)                \
    _varitem_type_(complex32,  complex*8)             \
    _varitem_type_(complex64,  complex*16)            \
    _varitem_type_(c_void_ptr, type(c_ptr))           \
    _varitem_type_(string,     type(DynamicString_t)) \
    _varitem_type_(gref,       type(GenericRef_t))


  ! For ifort and gfortran (both, in various versions) using real*16 or complex*32 results in REALLY fragile code!
  ! Calling the original subroutine (macro) for real128/complex128 causes strange segfaults in assingment or 
  !   constructor while assigning a real*16/complex*32 variable to another.
  ! In some cases, this is even that fragile that commenting out a line causes
  !   a segfault at some assignment BEFORE.
  ! Sorry, but at this point we enter the world of pure magic and I can only
  !   guess that this might be caused by some corruption of the MMX/SSE stack (used for copying a real*16/complex*32)
  !   or whatever.
  ! Even the intel inspector is perfectly fine with the test code (test_var_item.f90), throwing the sefaults!
  ! 
  ! FOR NOW: sorry, but best is to disable real128 and complex128 for VarItem storage by default.
  ! Some sunny day, we either find that nasty BUG or we finally got better compilers that tell us the truth.
  ! If so, we could reenable the types by defining the preprocessor flag VARITEM_REAL16

# if defined VARITEM_REAL16
#   define _TableOf_varitem_types_ \
      _TableOf_varitem_types_base_          \
      _varitem_type_(real128,    real*16)   \
      _varitem_type_(complex128, complex*32)
# else
#   define _TableOf_varitem_types_ \
      _TableOf_varitem_types_base_
# endif

#endif

