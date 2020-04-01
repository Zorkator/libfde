#ifndef __FDE_COMPAT_2_7_2_FPP
#define __FDE_COMPAT_2_7_2_FPP

! list of compatibility defines ...
#define file_basename   basename
#define file_dirname    dirname
! ...


! Warn the user ....
#warning #---------------------------------------------------------
#warning # Your code uses a FDE-compatibility header.
#warning # Please try to get rid of it by updating the following:
#warning # >> file_basename  - renamed to ->  basename
#warning # >> file_dirname   - renamed to ->  dirname
#warning #---------------------------------------------------------

#endif

