#ifndef __FORTRES_EXCEPTION__HPP
#define __FORTRES_EXCEPTION__HPP

#include "fortres/StringRef.hpp"
#include "fortres/portdef.h"
#include "fortres/exception_types.xpp"
#include "fortres/ppUtil.xpp"

typedef enum {
#   define _fortres_exception_type(_ident, _num) /* => */ _ident = _paste(0x,_num),
      _fortres_ExceptionTable
      _invalid_Exception = 0
#   undef  _fortres_exception_type
} ExceptionType;

class Context;

typedef void (*Procedure)( ... );
typedef void (*Synchronizer)( Context **, int );

_dllExport_C
void f_get_context( Context **context, int contextId );

_dllExport_C
void f_set_synchronizer( Synchronizer proc );

_dllExport_C
void f_set_traceproc( Procedure proc );

_dllExport_C
int f_try( int *catchList, StringRef *what, Procedure proc, ... );

_dllExport_C
void f_throw( int code, const StringRef *what );

extern
void f_throw_str( int code, std::string **msg );

_dllExport_C
void f_push_cleanup( Procedure proc );

_dllExport_C
void f_pop_cleanup( int exec );

#endif /* __FORTRES_EXCEPTION__HPP */

