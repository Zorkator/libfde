#ifndef __FDE_FUTURE_FPP
#define __FDE_FUTURE_FPP

#include "fortres/ppUtil.xpp"

# if defined __FDE_declare_future
#   define _fde_future(x)       _paste(fde_future_,x)
#   define _fde_mark_future(x)  _paste(fde_future_,x) => x
# else
#   define _fde_future(x)   x
# endif

#endif

