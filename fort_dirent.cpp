
#include <sys/stat.h>
#include <errno.h>

#define  _DLL_EXPORT_IMPLEMENTATION_
#include "fortres/dirent.hpp"

#if !defined _MSC_VER
  /* assume POSIX compatible compiler */
# include <unistd.h>
#	ifndef MAX_PATH
#		define MAX_PATH		4096
#	endif

#else
  /* sorry, it's M$ ... */
# include <windows.h>
# define getcwd( buf, size )    				GetCurrentDirectoryA( (DWORD)size, buf )
#	define strtok_r( str, delim, ctxt )		strtok_s( str, delim, ctxt )
#endif

static const char _pathDelim[] = PATH_DELIM;
static const char _pathSep[]   = PATH_DELIM other_PATH_DELIM;

void
make_cwd( std::string *cwd )
{
  cwd->resize( MAX_PATH );
  getcwd( &(*cwd)[0], cwd->capacity() );
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

