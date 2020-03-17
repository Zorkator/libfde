#ifndef __FORTRES_STDLIB_H
#define __FORTRES_STDLIB_H

#include <stdlib.h>

#include <fortres_config.h>

#if !defined HAVE_SETENV
  /* once again ... do the work M$ is not willing to do */
# define setenv( name, value, override )    (override || getenv(name) == NULL)? _putenv_s( name, value ) : 0
# define unsetenv( name )                   _putenv_s( name, "" )
#endif

#endif /* __FORTRES_STDLIB_H */

