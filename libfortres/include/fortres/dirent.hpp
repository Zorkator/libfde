#ifndef __FORTRES_DIRENT__HPP
#define __FORTRES_DIRENT__HPP

#include "fortres/StringRef.hpp"
#include "fortres/String.hpp"
#include "fortres/portdef.h"

#if defined HAVE_DIRENT_H
  /* assume POSIX compatible compiler */
# include <dirent.h>

# ifndef MAX_PATH
#   define MAX_PATH    4096
# endif

#else
  /* sorry, it's M$ ... */
# include <windows.h>
# include "fortres/dirent.h"

# define stat               _stat
#endif

_dllExport_C
void make_cwd( std::string *cwd );

/**
 * relpath functions
 */

_dllExport_C
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

/**
 * fortran callable wrappers for C++ relPath-functions ...
 **/

_dllExport_C
void f_make_relPath_from_to( StringRef *res, StringRef *src, StringRef *tgt );

_dllExport_C
void f_make_relPath_to( StringRef *res, StringRef *tgt );


/**
 * fortran callable wrappers for dirent-functions ...
 **/

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


/** C-convenience function isdir - obviously missing in dirent.h **/
_dllExport_C
int isdir( const char *path );

template <typename T>
int isdir( const T &path )
	{ return isdir( path.c_str() ); }


/**
 * so_filepath_of
 */

extern
size_t so_filepath_of( const void *addr, char buff[], size_t len );

extern
size_t so_filepath_of( const void *addr, std::string *pathBuff );

/** fortran callable wrapper for so_filepath_of ... **/
_dllExport_C
size_t f_so_filepath_of( const void *addr, StringRef *filePath );


/**
 * realpath
 */

extern
size_t make_realpath( const char *filePath, char *buff, size_t len );

extern
size_t make_realpath( const char *filePath, std::string *resolvedName );

inline
std::string
realpath_of( const char *filePath )
{
  std::string resolvedName;
  make_realpath( filePath, &resolvedName );
  return resolvedName;
}

/** fortran callable wrapper for realpath ... **/
_dllExport_C
size_t f_realpath( StringRef *filePath, StringRef *resolvedName );

#endif /*__FORTRES_DIRENT__HPP */

