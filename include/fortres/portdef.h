#ifndef __FORTRES_PORTDEF_H
#define __FORTRES_PORTDEF_H

# define _extern_C    extern "C"

# if defined _MSC_VER
    /* sorry, it's M$ ... */

#		if defined _DLL_EXPORT_IMPLEMENTATION_
#			define _dllExport       __declspec(dllexport)
#		else
#			define _dllExport       __declspec(dllimport)
#		endif

#   define _dllExport_C       _extern_C _dllExport

# else
    /* assume POSIX compatible compiler */

#   define _dllExport
#   define _dllExport_C

#endif

#endif /* __FORTRES_PORTDEF_H */

