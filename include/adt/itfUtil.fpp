#ifndef __ITFUTIL_FPP
#define __ITFUTIL_FPP

#include "adt/ppUtil.xpp"

! Note: some of the following macros generate excaimation marks (!) that intel's fpp usually interpret as comments.
! To make them work for intels VisualFortran (Windows), you've to
!  * switch off generic preprocessing
!  * add command line argument /fpp:"-f_com=no"
!  * check pp-output by adding command line argument /Qsave-temps
! or on linux command line ...
!  * add command line argument -fpp -allow nofpp-comments
!  * check pp-output by adding eihter -E (run fpp to stdout) or -preprocess-only

!
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

!
! depending on BUILT_TYPE and target system we need to define the
!   export macros SYM_EXPORT and PROC_EXPORT differently
!
# if BUILT_TYPE == SHARED_LIB
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




!# if BUILT_TYPE == SHARED_LIB
!    ! set export type of symbols and procedures
!#   if defined _WIN32
!#     define _EXPORTTYPE                        DLLEXPORT
!#   else
!#     define _EXPORTTYPE
!#   endif
!
!#   define _alias2( id )                         _str(id)
!#   define SYM_EXPORT( id )                     DEC$ ATTRIBUTES _EXPORTTYPE, ALIAS:_str(id)::id
!#   define PROC_EXPORT( id )                    DEC$ ATTRIBUTES _EXPORTTYPE, DECORATE,_CALLTYPE ALIAS:_alias2(id)::id
!
!    ! declare reference arguments of exported dll routines ...
!#   define _ARG_REFERENCE( a )                  DEC$ ATTRIBUTES REFERENCE::a
!#   define _ARG_REFERENCE2( a, b )              DEC$ ATTRIBUTES REFERENCE::a,b
!#   define _ARG_REFERENCE3( a, b, c )           DEC$ ATTRIBUTES REFERENCE::a,b,c
!#   define _ARG_REFERENCE4( a, b, c, d )        DEC$ ATTRIBUTES REFERENCE::a,b,c,d
!
!# else
!
!#   define SYM_EXPORT( id )                     
!#   define PROC_EXPORT( id )                    
!
!#   define _ARG_REFERENCE( id )                 
!#   define _ARG_REFERENCE2( a, b )              
!#   define _ARG_REFERENCE3( a, b, c )           
!#   define _ARG_REFERENCE4( a, b, c, d )        
!
!# endif
!
!!
!! provide some extended procedure exports for marking arguments
!!   to be passed by reference.
!!
!# define PROC_EXPORT_1REF( id, a )              PROC_EXPORT( id ) !_ARG_REFERENCE( a )
!# define PROC_EXPORT_2REF( id, a, b )           PROC_EXPORT( id ) !_ARG_REFERENCE2( a, b )
!# define PROC_EXPORT_3REF( id, a, b, c )        PROC_EXPORT( id ) !_ARG_REFERENCE3( a, b, c )
!# define PROC_EXPORT_4REF( id, a, b, c, d )     PROC_EXPORT( id ) !_ARG_REFERENCE4( a, b, c, d )

!
! define some compiler dependent flags
!
# if defined __INTEL_COMPILER
!   intel compiler detected
!   This allows us to use unions, cray-pointers, and the iso_c_binding module
#   define __HAS_UNION
#   define __HAS_CRAY_PTR
#   define __HAS_ISO_C

# else
!   some other fortran compiler
!   Assume support for cray-pointers and a iso_c_binding module
#   define __HAS_CRAY_PTR
#   define __HAS_ISO_C

# endif


#endif

