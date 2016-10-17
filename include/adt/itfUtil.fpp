#ifndef __ITFUTIL_FPP
#define __ITFUTIL_FPP

#include "adt/ppUtil.xpp"

! defining the following numeric constants allows us to use them in the
!   build configuration for setting BUILT_TYPE
! e.g.  -DBUILT_TYPE=SHARED_LIB
! By default assume APPLICATION
!
!                      . o O (need value != 0 for detecting user IDs)
# define DISABLED    -1
# define ENABLED      1
# define APPLICATION  2 
# define STATIC_LIB   3
# define SHARED_LIB   4

# ifndef BUILT_TYPE
!   BUILT_TYPE is not set assuming 'APPLICATION'
#   define BUILT_TYPE   APPLICATION
# endif

! if not set otherwise, configure EXPORT_SYMBOLS according to built type
# if !defined EXPORT_SYMBOLS
#   if BUILT_TYPE == SHARED_LIB
!     enable EXPORT_SYMBOLS
#     define EXPORT_SYMBOLS  ENABLED
#   else
!     disable EXPORT_SYMBOLS
#     define EXPORT_SYMBOLS  DISABLED
#   endif
# endif

!
! if exports are enabled we need to define the export macros
!   _SYM_EXPORT and _PROC_EXPORT according to compiler and system.
!
# if EXPORT_SYMBOLS == ENABLED
!   prepare for building shared library

#   if defined __INTEL_COMPILER
!     using ifort
#     define _alias(id)                           _str(_paste(id,_))

#     if defined _WIN32
!       using windows
#       define _PROC_EXPORT( id )               DEC$ ATTRIBUTES DLLEXPORT::id
#       define _CPROC_EXPORT( id )              DEC$ ATTRIBUTES DLLEXPORT::id
#       define _SYM_EXPORT( id )                DEC$ ATTRIBUTES DLLEXPORT,ALIAS:_str(id)::id
#     else
!       assuming some Linux
#       define _PROC_EXPORT( id )               DEC$ ATTRIBUTES ALIAS:_alias(id)::id
#       define _CPROC_EXPORT( id )              ~~> any DEC would conflict with bind(c)
#       define _SYM_EXPORT( id )                DEC$ ATTRIBUTES ALIAS:_str(id)::id
#     endif

#     define _ARG_REFERENCE( a )                DEC$ ATTRIBUTES REFERENCE::a
#     define _ARG_REFERENCE2( a, b )            DEC$ ATTRIBUTES REFERENCE::a,b
#     define _ARG_REFERENCE3( a, b, c )         DEC$ ATTRIBUTES REFERENCE::a,b,c
#     define _ARG_REFERENCE4( a, b, c, d )      DEC$ ATTRIBUTES REFERENCE::a,b,c,d
#     define _ARG_REFERENCE5( a, b, c, d, e )   DEC$ ATTRIBUTES REFERENCE::a,b,c,d,e

#   else
!     not using ifort ...
!     TODO for other compilers?
#   endif
# endif


# ifndef _PROC_EXPORT
!   If we end up here, we're either not building a shared library or we use a compiler that doesn't hide shared library symbols.
!   In either case, exports are not needed or they don't make sense.
!   Thus, we just disable all export macros by defining them empty.
#   define _PROC_EXPORT( id )
#   define _CPROC_EXPORT( id )
#   define _SYM_EXPORT( id )
#   define _ARG_REFERENCE( a )
#   define _ARG_REFERENCE2( a, b )
#   define _ARG_REFERENCE3( a, b, c )
#   define _ARG_REFERENCE4( a, b, c, d )
#   define _ARG_REFERENCE5( a, b, c, d, e )
# endif


# if defined __INTEL_COMPILER
#   define _CPROC( id )              DEC$ ATTRIBUTES C::id
#   define _use_if_INTEL(mod)        use mod
# else
!     not using ifort ...
#   define _CPROC( id )
#   define _use_if_INTEL(mod)
# endif


!   Little helper macro for defining C-name binding.
!   This can be VERY useful when fighting portably with fixed format limits!
#   define _cID(id)   bind(C,name=_str(id))

#endif

