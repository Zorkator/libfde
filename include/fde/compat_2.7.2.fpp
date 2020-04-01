#ifndef __FDE_COMPAT_2_7_2_FPP
#define __FDE_COMPAT_2_7_2_FPP

#define _FDE_declare_future
#include "fde/future.fpp"

! list of compatibility defines ...
#define file_basename   basename
#define file_dirname    dirname
! ...

#define _disarm_fde_clashes \
    use fde_string, only:   \
        _fde_from_future(filename) ,\
        _fde_from_future(basename)


! Warn the user ....
#warning #---------------------------------------------------------
#warning # Your code uses a FDE-compatibility header.
#warning # Please try to get rid of it by updating the following:
#warning # >> file_basename  - renamed to ->  basename
#warning # >> file_dirname   - renamed to ->  dirname
#warning #
#warning # >> Sorry for the name-clashes that might occur :-/
#warning #---------------------------------------------------------

#endif

