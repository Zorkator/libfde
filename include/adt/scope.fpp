#ifndef __ADT_SCOPE_FPP
#define __ADT_SCOPE_FPP

#include "adt/ppUtil.xpp"
#include "adt/string.fpp"

# define _get_scope( parent, scopeId ) \
    getScope( parent, _strip(scopeId) )

# define _get_scope2( parent, a, b ) \
    getScope( parent, _strip(a), _strip(b) )

# define _get_scope3( parent, a, b, c ) \
    getScope( parent, _strip(a), _strip(b), _strip(c) )

# define _get_scope4( parent, a, b, c, d ) \
    getScope( parent, _strip(a), _strip(b), _strip(c), _strip(d) )

# define _get_scope5( parent, a, b, c, d, e ) \
    getScope( parent, _strip(a), _strip(b), _strip(c), _strip(d), _strip(e) )

# define _get_scope6( parent, a, b, c, d, e, f ) \
    getScope( parent, _strip(a), _strip(b), _strip(c), _strip(d), _strip(e), _strip(f) )

# define _get_scope7( parent, a, b, c, d, e, f, g ) \
    getScope( parent, _strip(a), _strip(b), _strip(c), _strip(d), _strip(e), _strip(f), _strip(g) )

# define _get_scope8( parent, a, b, c, d, e, f, g, h ) \
    getScope( parent, _strip(a), _strip(b), _strip(c), _strip(d), _strip(e), _strip(f), _strip(g), _strip(h) )

# define _get_scope9( parent, a, b, c, d, e, f, g, h, i ) \
    getScope( parent, _strip(a), _strip(b), _strip(c), _strip(d), _strip(e), _strip(f), _strip(g), _strip(h), _strip(i) )

!
! The following definitions might be changed by the native code using libadt.
!  __rootScope__ : a string, variable or pointer identifying the code's root scope.
!  __hookScope__ : a string, variable or pointer identifying the code's hook scope.
!  __sym2str__   : a macro converting a symbol to a string.
!  __istat__     : the integer status variable used by the [DE]ALLOCATE macros below.
!
! It should be ok to use the default configuration.
! For customizing these, remember to predefine the macros BEFORE including this file!
!
# if !defined __rootScope__
#   define __rootScope__        "__adt_process__"
# endif

# if !defined __hookScope__
#   define __hookScope__        _get_scope( __rootScope__, "hooks" )
# endif

# if !defined __sym2str__  
#   define __sym2str__(sym)     _strip(_str(sym))
# endif

# if !defined __istat__
#   define __istat__            istat
# endif


# define _file_scopeId() \
    file_basename( __FILE__ )

# define _file_scope_in( parent ) \
    _get_scope( parent, _file_scopeId() )

# define _file_scope() \
    _file_scope_in( __rootScope__ )


# define _set_scopeSymbol_as( parent, sym, id ) \
    call set( parent, id, Item_of(ref_of(sym)) )

# define _set_scopeSymbol( parent, sym ) \
    _set_scopeSymbol_as( parent, sym, _str(sym) )

# define _bind_scopeSymbol_as( parent, sym, id ) \
    call set( parent, id, Item_of(ref_of(sym, bind=.true.)) )

# define _bind_scopeSymbol( parent, sym ) \
    _bind_scopeSymbol_as( parent, sym, _str(sym) )

# define _set_scopeValue_as( parent, sym, id ) \
    call set( parent, id, Item_of(sym) )

# define _set_scopeValue( parent, sym ) \
    _set_scopeValue_as( parent, sym, _str(sym) )


# define _remove_scopeSymbol( parent, id ) \
    call remove( parent, id )


!
! The following definitions simplify the use of adt scopes
! In case of name clashes define _ADT_SCOPE_NO_SIMPLIFIED to avoid them.
!

# if !defined _ADT_SCOPE_NO_SIMPLIFIED

#   define _linkSymbol_as( scope, sym, id ) \
      _set_scopeSymbol_as( scope, sym, id )

#   define _linkSymbol( scope, sym ) \
      _linkSymbol_as( scope, sym, __sym2str__(sym) )

#   define _bindSymbol_as( scope, sym, id ) \
      _bind_scopeSymbol_as( scope, sym, id )

#   define _bindSymbol( scope, sym ) \
      _bindSymbol_as( scope, sym, __sym2str__(sym) )

#   define _setSymbol_as( scope, sym, id ) \
      _set_scopeValue_as( scope, sym, id )

#   define _setSymbol( scope, sym ) \
      _setSymbol_as( scope, sym, __sym2str__(sym) )


#   define _callHook( hookId ) \
      call invokeCallback( __hookScope__, hookId )

#   define _callArgHook( hookId, argScope ) \
      call invokeCallback( __hookScope__, hookId, argScope )
      

#   define _removeSymbol( scope, sym ) \
      _remove_scopeSymbol( scope, __sym2str__(sym) )


!
! definine allocation macros these are needed to update
!   a scope symbol everytime it get [re-]allocated
!
# define __codefrag_ALLOCATE_hidden( sym, shape, _st ) \
      allocate( sym shape, stat=_st )

# define __codefrag_DEALLOCATE_hidden( sym, _st )      \
      deallocate( sym, stat=_st )

# define __codefrag_ALLOCATE_visible_as( scope, sym, shape, id, _st ) \
      allocate( sym shape, stat=_st )                                ;\
      _linkSymbol_as( scope, sym, id )

# define __codefrag_ALLOCATE_visible( scope, sym, shape, _st )        \
      __codefrag_ALLOCATE_visible_as( scope, sym, shape, __sym2str__(sym), _st )

# define __codefrag_DEALLOCATE_visible( scope, sym, _st ) \
      deallocate( sym, stat=_st )                        ;\
      _removeSymbol( scope, sym )



!###########################################
! variant using status variable __istat__
!###########################################

# define _ALLOCATE( sym, shape ) \
      __codefrag_ALLOCATE_hidden( sym, shape, __istat__ )
    
# define _assert_ALLOCATE( sym, shape )                      \
      if (.not. allocated(sym)) then                        ;\
        __codefrag_ALLOCATE_hidden( sym, shape, __istat__ ) ;\
      end if

# define _REALLOCATE( sym, shape )                           \
      select case (0); case default                         ;\
        if (allocated(sym)) deallocate(sym)                 ;\
        __codefrag_ALLOCATE_hidden( sym, shape, __istat__ ) ;\
      end select
      
# define _DEALLOCATE( sym )             \
      __codefrag_DEALLOCATE_hidden( sym, __istat__ )


# define _ALLOCATE_visible_as( scope, sym, shape, id )                      \
      select case (0); case default                                        ;\
        __codefrag_ALLOCATE_visible_as( scope, sym, shape, id, __istat__ ) ;\
      end select
    
# define _ALLOCATE_visible( scope, sym, shape ) \
      _ALLOCATE_visible_as( scope, sym, shape, __sym2str__(sym) )

# define _assert_ALLOCATE_visible( scope, sym, shape )               \
      if (.not. allocated(sym)) then                                ;\
        __codefrag_ALLOCATE_visible( scope, sym, shape, __istat__ ) ;\
      end if

# define _REALLOCATE_visible_as( scope, sym, shape, id )                    \
      select case (0); case default                                        ;\
        if (allocated(sym)) deallocate(sym)                                ;\
        __codefrag_ALLOCATE_visible_as( scope, sym, shape, id, __istat__ ) ;\
      end select
      
# define _REALLOCATE_visible( scope, sym, shape )    \
      _REALLOCATE_visible_as( scope, sym, shape, __sym2str__(sym) )

# define _DEALLOCATE_visible( scope, sym )                      \
      select case (0); case default                            ;\
        __codefrag_DEALLOCATE_visible( scope, sym, __istat__ ) ;\
      end select

# endif

#endif

