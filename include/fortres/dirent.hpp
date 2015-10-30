#ifndef __FORTRES_DIRENT__HPP
#define __FORTRES_DIRENT__HPP

#include "fortres/StringRef.hpp"
#include "fortres/portdef.h"

#if !defined _MSC_VER
  /* assume POSIX compatible compiler */
# include <dirent.h>

# define PATH_DELIM         "/"
# define other_PATH_DELIM   "\\"

#else
  /* sorry, it's M$ ... */
#	include "fortres/dirent.h"

# define PATH_DELIM         "\\"
# define other_PATH_DELIM   "/"
#endif

_dllExport_C
int f_opendir( DIR **dir, StringRef *name );

_dllExport_C
int f_readdir( DIR *dir, StringRef *dirEntry );

_dllExport_C
void f_closedir( DIR *dir, uint32_t *stat );

_dllExport_C
void f_rewinddir( DIR *dir, uint32_t *stat );

#endif /*__FORTRES_DIRENT__HPP */

