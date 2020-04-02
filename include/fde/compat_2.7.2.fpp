#ifndef __FDE_COMPAT_2_7_2_FPP
#define __FDE_COMPAT_2_7_2_FPP

#define _FDE_declare_future
#include "fde/future.fpp"

#define _fde_postpone_future \
    use fde_string, only:    \
        _fde_from_future(filename) ,\
        _fde_from_future(basename) ,\
        _fde_from_future(dirname)

! Warn the user ....
#warning #---------------------------------------------------------
#warning # Your code uses a FDE-compatibility header.
#warning # Please try to get rid of it by updating the following:
#warning # >> file_basename  - renamed to ->  basename
#warning # >> file_dirname   - renamed to ->  dirname
#warning #
#warning # For now you can use the compatibility modules instead:
#warning # >> use fde_string_compat_2_7_2
#warning #---------------------------------------------------------

#endif

