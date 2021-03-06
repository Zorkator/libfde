#include <fortres_config.h>

#include <sys/stat.h>
#include <errno.h>

#define  _DLL_EXPORT_IMPLEMENTATION_
#include "fortres/dirent.hpp"

#if defined HAVE_STRING_SECURE
# define strtok_r( str, delim, ctxt )   strtok_s( str, delim, ctxt )
#endif

#if defined HAVE_WINDOWS_H
# include <windows.h>
  /* sorry, it's M$ ... */
# define getcwd( buf, size )            GetCurrentDirectoryA( (DWORD)size, buf )
#else
  /* assume POSIX compatible compiler */
# include <stdlib.h>
# include <unistd.h>
#endif

#if defined HAVE_DLFCN_H
# include <dlfcn.h>
#endif

static const char _pathDelim[] = PATH_SEP;
static const char _pathSep[]   = PATH_SEP_WINDOWS PATH_SEP_UNIX;


void
make_cwd( std::string *cwd )
{
  cwd->resize( MAX_PATH );
  getcwd( &(*cwd)[0], cwd->capacity() );
  cwd->resize( strnlen( &(*cwd)[0], cwd->capacity() ) );
}


void
make_relPath_from_to( std::string *res, char *src, char *tgt )
{
  if (*src && *tgt)
  {
    char *t1, *t1n;
    char *t2, *t2n;

    t1  = strtok_r( src, _pathSep, &t1n );
    t2  = strtok_r( tgt, _pathSep, &t2n );
    res->assign(".");

    // skip identical parts ...
    while (t1 != NULL && t2 != NULL)
    {
      if (strcmp( t1, t2 ))
        { break; }
      t1 = strtok_r( NULL, _pathSep, &t1n );
      t2 = strtok_r( NULL, _pathSep, &t2n );
    }

    // make way up starting from src ...
    while (t1 != NULL)
    {
      res->append( _pathDelim ).append( ".." );
      t1 = strtok_r( NULL, _pathSep, &t1n );
    }

    // make way down to tgt ...
    while (t2 != NULL)
    {
      res->append( _pathDelim ).append( t2 );
      t2 = strtok_r( NULL, _pathSep, &t2n );
    }
  }
  else
    { res->assign( tgt ); }
}


_dllExport_C
void
f_make_relPath_from_to( StringRef *res, StringRef *src, StringRef *tgt )
{
  std::string _res;
  make_relPath_from_to( &_res, const_cast<char *>(src->str().c_str()), const_cast<char *>(tgt->str().c_str()) );
  *res = _res;
}


_dllExport_C
void
f_make_relPath_to( StringRef *res, StringRef *tgt )
{
  std::string _res, _src;
  make_cwd( &_src );
  make_relPath_from_to( &_res, const_cast<char *>(_src.c_str()), const_cast<char *>(tgt->str().c_str()) );
  *res = _res;
}


_dllExport_C
int
f_opendir( DIR **dir, StringRef *name )
{
  std::string dirName = name->str();
  *dir = opendir( dirName.c_str() );
  return (*dir != NULL);
}


_dllExport_C
int
f_readdir( DIR *dir, StringRef *dirEntry )
{
  struct dirent *entry = readdir( dir );
  int            isOk  = (entry != NULL);

  if (isOk)
    { dirEntry->referTo( entry->d_name ); }
  return isOk;
}


_dllExport_C
void
f_closedir( DIR **dir, uint32_t *stat )
{
  uint32_t result = closedir( *dir );
  *dir = NULL;
  if (stat != NULL)
    { *stat = result; }
}


_dllExport_C
void
f_rewinddir( DIR *dir, uint32_t *stat )
{
  rewinddir( dir );
  if (stat != NULL)
    { *stat = errno; }
}


_dllExport_C
int
f_isdir( StringRef *pathStr )
{
  struct stat s;
  return (!stat( pathStr->trimmed().str().c_str(), &s ) && (s.st_mode & S_IFDIR));
}



size_t
so_filepath_of( const void *addr, char buff[], size_t len )
{
  size_t res = 0;

#if defined HAVE_LIBLOADERAPI_H
  HMODULE hdl = NULL;
  if (GetModuleHandleExA( GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT, (LPCSTR)addr, &hdl ))
    { res = GetModuleFileNameA( hdl, buff, (DWORD)len ); }

#elif defined HAVE_DLFCN_H
  Dl_info info;
  if (dladdr( const_cast<void *>(addr), &info ))
  {
    if (len > 0)
    {
      res = std::min( len-1, strlen( info.dli_fname ) );
      memcpy( buff, info.dli_fname, res );
      buff[res] = '\0'; //< make sure string is terminated!
    }
  }

#else
  #error "Neither HAVE_WINDOWS_H nor HAVE_DLFCN_H"
#endif
  return res;
}


_dllExport_C
size_t
f_so_filepath_of( const void *addr, StringRef *filePath )
{
  size_t len = so_filepath_of( addr, filePath->buffer(), filePath->length() );
  filePath->pad( len );
  return len;
}


size_t
make_realpath( const char *filePath, char *buff, size_t len )
{
  size_t tgtLen = 0;

#if defined HAVE_WINDOWS_H
  tgtLen = GetFullPathName( filePath, static_cast<DWORD>(len), buff, NULL );
#else
  char *ptr;

  if ((ptr = realpath( filePath, NULL )) != NULL)
  {
    tgtLen = std::min( len-1, strlen(ptr) );
    memcpy( buff, ptr, tgtLen );
    free( ptr );
  }
#endif
  return tgtLen;
}

size_t
make_realpath( const char *filePath, std::string *resolvedName )
{
  size_t len;
  resolvedName->resize( MAX_PATH );
  len = make_realpath( filePath, &(*resolvedName)[0], resolvedName->capacity() );
  resolvedName->resize( len );
  return len;
}


_dllExport_C
size_t
f_realpath( StringRef *filePath, StringRef *resolvedName )
{
  size_t len = make_realpath( filePath->buffer(), resolvedName->buffer(), resolvedName->length() );
  if (len < resolvedName->length())
    { memset( resolvedName->buffer() + len, ' ', resolvedName->length() - len ); }
  return len;
}



