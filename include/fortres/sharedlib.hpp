#ifndef __FORTRES_SHAREDLIB__HPP
#define __FORTRES_SHAREDLIB__HPP

#include "fortres/StringRef.hpp"
#include "fortres/portdef.h"

typedef void (*PluginInfoHandler)( StringRef *id, StringRef *filePath );

_dllExport_C
void f_set_plugin_path( StringRef *path, StringRef *chkSym );

_dllExport_C
void f_register_plugin( StringRef *pluginId );

_dllExport_C
void f_plugin_iterate( PluginInfoHandler handler );

_dllExport_C
void * f_get_symbol_of( StringRef *pluginId, StringRef *symId );

_dllExport_C
void * f_get_procedure_of( StringRef *pluginId, StringRef *symId );

_dllExport_C
int f_try_call_of( StringRef *pluginId, StringRef *symId );

_dllExport_C
void f_call_plugin( StringRef *pluginId, StringRef *symId );

extern
size_t so_filepath_of( const void *addr, char buff[], size_t len );

_dllExport_C
size_t f_so_filepath_of( const void *addr, StringRef *filePath );

#endif /* __FORTRES_SHAREDLIB__HPP */

