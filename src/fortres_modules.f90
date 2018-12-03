

#define _MODULENAME_stringref       fde_stringref
#define _MODULENAME_dirent          fde_dirent
#define _MODULENAME_try_interface   fde_try_interface
#define _MODULENAME_exception       fde_exception
#define _MODULENAME_plugin          fde_plugin
#define _MODULENAME_tracestack      fde_tracestack

# include "fortres/exception.fmod"
  ! include "fortres/stringref.fmod"  !< dependency of exception.fmod!
  ! include "fortres/tracestack.fmod" !< dependency of exception.fmod!
# include "fortres/plugin.fmod"
# include "fortres/dirent.fmod"

