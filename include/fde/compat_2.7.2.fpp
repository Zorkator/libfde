#ifndef __FDE_COMPAT_2_7_2_FPP
#define __FDE_COMPAT_2_7_2_FPP

! allow using future declarations ...
#define __FDE_declare_future
#include "fde/future.fpp"


! Warn the user ....
# warning #---------------------------------------------------------
# warning # Your code uses a FDE-compatibility header.
# warning # Please try to get rid of it by updating the following:
# warning # >> file_basename  - renamed to ->  basename
# warning # >> file_dirname   - renamed to ->  dirname
# warning #
# warning # For now you are using the compatibility modules:
# warning # >> use fde_string_compat_2_7_2
# warning #---------------------------------------------------------

# define fde_string  fde_string_compat_2_7_2

#endif

