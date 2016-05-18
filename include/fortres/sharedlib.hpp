#ifndef __FORTRES_SHAREDLIB__HPP
#define __FORTRES_SHAREDLIB__HPP

#include "fortres/StringRef.hpp"
#include "fortres/portdef.h"

typedef void (*PluginInfoHandler)( StringRef *id, StringRef *filePath );

_dllExport_C
void f_plugin_set_path( StringRef *path, StringRef *libPath, StringRef *chkSym );

_dllExport_C
void f_plugin_register( StringRef *pluginId );

_dllExport_C
void f_plugin_iterate( PluginInfoHandler handler );

_dllExport_C
void f_plugin_filePath_to_id( StringRef *filePath, StringRef *id );

_dllExport_C
void * f_plugin_sym( StringRef *pluginId, StringRef *symId );

_dllExport_C
void f_plugin_call( StringRef *pluginId, StringRef *symId );

_dllExport_C
void * f_plugin_try_sym( StringRef *pluginId, StringRef *symId );

_dllExport_C
int f_plugin_try_call( StringRef *pluginId, StringRef *symId );

#endif /* __FORTRES_SHAREDLIB__HPP */

