#ifndef __FORTRES_EXCEPTION__HPP
#define __FORTRES_EXCEPTION__HPP

#include "fortres/StringRef.hpp"
#include "fortres/portdef.h"
#include "fortres/tracestack.hpp"
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
void f_format_exception( StringRef *buf, int code, StringRef *msg );

_dllExport_C
void f_get_context( Context **context, int contextId );

_dllExport_C
void f_set_synchronizer( Synchronizer proc );

_dllExport_C
void f_set_traceproc( TraceProcedure traceProc );

_dllExport_C
int f_try( int *catchList, StringRef *what, TraceProcedure tp, Procedure proc, ... );

_dllExport_C
int f_try_v( int *catchList, StringRef *what, TraceProcedure tp, Procedure proc, va_list vaArgs );

_dllExport_C
void f_throw( int code, const StringRef *what );

extern
void f_throw_str( int code, std::string **msg );

inline
void f_throw( int code, const char *msg )
{
  StringRef ref( msg );
  f_throw( code, &ref );
}


_dllExport_C
void f_push_cleanup( Procedure proc );

_dllExport_C
void f_pop_cleanup( int exec );

#endif /* __FORTRES_EXCEPTION__HPP */

