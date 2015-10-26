#ifndef __FORTRES_PORTDEF_H
#define __FORTRES_PORTDEF_H

# define _extern_C    extern "C"

# if defined _MSC_VER
    /* sorry, it's M$ ... */
#   define _dllExport         __declspec(dllexport)
#   define _dllExport_C       _extern_C _dllExport

    // define missing types
    typedef unsigned __int32  uint32_t;

# else
    /* assume POSIX compatible compiler */

#   include <stdint.h>
#   define _dllExport
#   define _dllExport_C

#endif

#endif /* __FORTRES_PORTDEF_H */

