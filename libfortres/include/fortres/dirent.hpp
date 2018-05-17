#ifndef __FORTRES_DIRENT__HPP
#define __FORTRES_DIRENT__HPP

#include "fortres/StringRef.hpp"
#include "fortres/String.hpp"
#include "fortres/portdef.h"

#if !defined _MSC_VER
  /* assume POSIX compatible compiler */
# include <dirent.h>

# define PATH_DELIM         "/"
# define other_PATH_DELIM   "\\"

#else
  /* sorry, it's M$ ... */
# include "fortres/dirent.h"

# define PATH_DELIM         "\\"
# define other_PATH_DELIM   "/"
#endif

extern
void make_cwd( std::string *cwd );

extern
void make_relPath_from_to( std::string *res, char *src, char *tgt );

inline
std::string
relPath_from_to( const char *src, const char *tgt )
{
  std::string res, _src(src), _tgt(tgt);
  make_relPath_from_to( &res, const_cast<char *>(_src.c_str()), const_cast<char *>(_tgt.c_str()) );
  return res;
}

inline
std::string
relPath_to( const char *tgt )
{
  std::string res, _src, _tgt(tgt);
  make_cwd( &_src );
  make_relPath_from_to( &res, const_cast<char *>(_src.c_str()), const_cast<char *>(_tgt.c_str()) );
  return res;
}

_dllExport_C
void f_make_relPath_from_to( StringRef *res, StringRef *src, StringRef *tgt );

_dllExport_C
void f_make_relPath_to( StringRef *res, StringRef *tgt );

_dllExport_C
int f_opendir( DIR **dir, StringRef *name );

_dllExport_C
int f_readdir( DIR *dir, StringRef *dirEntry );

_dllExport_C
void f_closedir( DIR **dir, uint32_t *stat );

_dllExport_C
void f_rewinddir( DIR *dir, uint32_t *stat );

_dllExport_C
int f_isdir( StringRef *pathStr );

extern
size_t so_filepath_of( const void *addr, char buff[], size_t len );

_dllExport_C
size_t f_so_filepath_of( const void *addr, StringRef *filePath );

extern
size_t make_realpath( const char *filePath, char *buff, size_t len );

extern
size_t make_realpath( const char *filePath, std::string *resolvedName );

_dllExport_C
size_t f_realpath( StringRef *filePath, StringRef *resolvedName );

inline
std::string
realpath_of( const char *filePath )
{
  std::string resolvedName;
  make_realpath( filePath, &resolvedName );
  return resolvedName;
}

#endif /*__FORTRES_DIRENT__HPP */

