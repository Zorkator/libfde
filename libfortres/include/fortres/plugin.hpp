#ifndef __FORTRES_SHAREDLIB__HPP
#define __FORTRES_SHAREDLIB__HPP

#include "fortres/StringRef.hpp"
#include "fortres/portdef.h"

typedef void (*PluginInfoHandler)( StringRef *id, StringRef *filePath, int *state );
typedef int  (*SOInfoHandler)( StringRef *id, const void *handle );

_dllExport_C
void f_plugin_set_path( StringRef *path, StringRef *libPath, StringRef *chkSym );

_dllExport_C
void f_plugin_register( StringRef *pluginId, StringRef *id, int *isEnabled );

_dllExport_C
void f_plugin_register_so( StringRef *fileName, StringRef *id );

inline
void plugin_register_so( const char *fileName, const char *id )
{
	StringRef fileNameRef( fileName ), idRef( id );
	f_plugin_register_so( &fileNameRef, &idRef );
}

_dllExport_C
int  f_plugin_set_enabled( StringRef *pluginId, int *isEnabled );

_dllExport_C
void f_plugin_iterate( PluginInfoHandler handler );

_dllExport_C
void f_plugin_filePath_to_id( StringRef *filePath, StringRef *id );

_dllExport_C
int f_plugin_available( StringRef *pluginId, StringRef *symId );

_dllExport_C
void * f_plugin_sym( StringRef *pluginId, StringRef *symId );

_dllExport_C
void f_plugin_call( StringRef *pluginId, StringRef *symId );

_dllExport_C
void * f_plugin_try_sym( StringRef *pluginId, StringRef *symId );

_dllExport_C
int f_plugin_try_call( StringRef *pluginId, StringRef *symId );

_dllExport_C
int f_plugin_iterate_so( SOInfoHandler handler );

_dllExport_C
int f_plugin_unload( StringRef *pluginId );

#endif /* __FORTRES_SHAREDLIB__HPP */

