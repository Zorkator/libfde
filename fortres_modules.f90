

#define _MODULENAME_stringref       adt_stringref
#define _MODULENAME_dirent          adt_dirent
#define _MODULENAME_try_interface   adt_try_interface
#define _MODULENAME_exception       adt_exception
#define _MODULENAME_plugin          adt_plugin
#define _MODULENAME_tracestack      adt_tracestack

# include "fortres/exception.fmod"
  !# include "fortres/stringref.fmod"  !< dependency of exception.fmod!
  !# include "fortres/tracestack.fmod" !< dependency of exception.fmod!
# include "fortres/sharedlib.fmod" !< TODO: rename to plugin
# include "fortres/dirent.fmod"

