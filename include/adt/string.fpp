#ifndef __ADT_STRING_FPP
#define __ADT_STRING_FPP

# define _strip(st) \
    trim(adjustl(st))

# define _this_file_basename() \
    _strip(file_basename( __FILE__ ))

#endif

