#ifndef __FDE_ITEM_FPP
#define __FDE_ITEM_FPP

#include "fortres/ppUtil.xpp"

# define _TableOf_item_types_base_ \
    _item_type_(bool1,      logical*1)      \
    _item_type_(bool2,      logical*2)      \
    _item_type_(bool4,      logical*4)      \
    _item_type_(bool8,      logical*8)      \
    _item_type_(int1,       integer*1)      \
    _item_type_(int2,       integer*2)      \
    _item_type_(int4,       integer*4)      \
    _item_type_(int8,       integer*8)      \
    _item_type_(real4,      real*4)         \
    _item_type_(real8,      real*8)         \
    _item_type_(complex8,   complex*8)      \
    _item_type_(complex16,  complex*16)     \
    _item_type_(c_void_ptr, type(c_ptr))    \
    _item_type_(string,     type(String_t)) \
    _item_type_(ref,        type(Ref_t))


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
  ! FOR NOW: sorry, but best is to disable real*16 and complex*32 for Item storage by default.
  ! Some sunny day, we either find that nasty BUG or we finally got better compilers that tell us the truth.
  ! If so, we could reenable the types by defining the preprocessor flag ITEM_REAL16

# if defined ITEM_REAL16
#   define _TableOf_item_types_ \
      _TableOf_item_types_base_         \
      _item_type_(real16,    real*16)   \
      _item_type_(complex32, complex*32)
# else
#   define _TableOf_item_types_ \
      _TableOf_item_types_base_
# endif

#endif

