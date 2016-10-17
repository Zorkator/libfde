

#define _MODULENAME_stringref   adt_stringref
#define _MODULENAME_dirent      adt_dirent
#define _MODULENAME_exception   adt_exception
#define _MODULENAME_plugin      adt_plugin
#define _MODULENAME_tracestack  adt_tracestack

!# include "fortres/stringref.fmod" !< already included by exception.fmod!
# include "fortres/exception.fmod"
# include "fortres/sharedlib.fmod" !< TODO: rename to plugin
# include "fortres/tracestack.fmod"
# include "fortres/dirent.fmod"

#undef _MODULENAME_stringref
#undef _MODULENAME_dirent
#undef _MODULENAME_exception
#undef _MODULENAME_plugin
#undef _MODULENAME_tracestack

