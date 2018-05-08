#ifndef __ADT_STRING_FPP
#define __ADT_STRING_FPP

#include "fortres/ppUtil.xpp"

# define _strip(st) \
    trim(adjustl(st))

# define _this_file_basename() \
    _strip(file_basename( __FILE__ ))


! The following definition might be changed by the native code using libadt.
!  __sym2str__: a macro converting a symbol to a string.

# if !defined __sym2str__
#   define __sym2str__(sym)  _strip(_str(sym))
# endif

#endif

