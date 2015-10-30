
#include <sys/stat.h>
#include <errno.h>

#define  _DLL_EXPORT_IMPLEMENTATION_
#include "fortres/dirent.hpp"

/* TODO:
 *  - specify and implement FORTRAN compatible interface functions
 *  - allow access by providing FORTRAN interfaces
 *
 
  type, bind(C) :: DIR_t
    private
    type(c_ptr) :: ptr
  end type
 
  interface 
    logical
    function opendir( dir, dirPath )
      type(DIR_t),      intent(out) :: dir
      type(StringRef_t), intent(in) :: dirPath
    end function

    logical
    function readdir( dir, dirEntry )
      type(DIR_t),       intent(inout) :: dir
      type(StringRef_t), intent(inout) :: dirEntry
    end function

    subroutine closedir( dir, stat )
    type(DIR_t), intent(inout) :: dir
    logical*4,        optional :: stat
    end subroutine

    subroutine rewinddir( dir, stat )
      type(DIR_t), intent(inout) :: dir
      logical,          optional :: stat
    end subroutine

    ! some more here ? ...
    logical
    function is_directory( dirEntry )
      type(StringRef_t), intent(in) :: dirEntry
    end function

  end interface


! usage

  type(DIR_t)       :: dir
  type(StringRef_t) :: dirEntry
  logical           :: stat

  if (opendir( dir, str("./some_directory/path") )) then
    do while (readdir( dir, dirEntry ))
      write(6,*) str(dirEntry)
    end do
    call rewinddir( dir )
    call closedir( dir )
  else
    write(0,*) "unknown directory"
  endif 
*/



_dllExport_C
int
f_opendir( DIR **dir, StringRef *name )
{
  std::string dirName = name->str();
  *dir = opendir( dirName.c_str() );
  return (dir != NULL);
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
f_closedir( DIR *dir, uint32_t *stat )
{
  uint32_t result = closedir( dir );
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

