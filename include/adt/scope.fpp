#ifndef __ADT_SCOPE_FPP
#define __ADT_SCOPE_FPP

#include "adt/allocate.fpp"

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


!########################################################################################
! The following definitions might be changed by the native code using libadt.
!  __rootLocator__: comma-separated string list identifying the root scope
!  __hookLocator__: comma-separated string list identifying the hook scope
!  __rootScope__  : variable or pointer identifying the code's root scope.
!  __hookScope__  : variable or pointer identifying the code's hook scope.
!
! It should be ok to use the default configuration.
! For customizing these, remember to predefine the macros BEFORE including this file!
!########################################################################################

# if !defined __rootLocator__
#   define __rootLocator__      "__adt_process__"
# endif

# if !defined __hookLocator__
#   define __hookLocator__      __rootLocator__, "hooks"
# endif

# if !defined __rootScope__
#   define __rootScope__        getScope( __rootLocator__ )
# endif

# if !defined __hookScope__
#   define __hookScope__        getScope( __hookLocator__ )
# endif

# define _file_scopeId() \
    file_basename( __FILE__ )

# define _file_scope_in( parent ) \
    _get_scope( parent, _file_scopeId() )

# define _file_scope() \
    _file_scope_in( __rootScope__ )

# define _root_scope() \
    __rootScope__

# define _hook_scope() \
    __hookScope__


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

!
! set and bind macros specialized for arrays ...
!
# define _set_scopeArray_as( parent, sym, id ) \
    call set( parent, id, Item_of(ref_of(sym, lb=lbound(sym), ub=ubound(sym))) )

# define _set_scopeArray( parent, sym ) \
    _set_scopeArray_as( parent, sym, _str(sym) )

# define _bind_scopeArray_as( parent, sym, id ) \
    call set( parent, id, Item_of(ref_of(sym, bind=.true., lb=lbound(sym), ub=ubound(sym))) )

# define _bind_scopeArray( parent, sym ) \
    _bind_scopeArray_as( parent, sym, _str(sym) )


# define _remove_scopeSymbol( parent, id ) \
    call remove( parent, id )


# if !defined _ADT_SCOPE_NO_SIMPLIFIED
!##############################################################################
! The following definitions simplify the use of adt scopes
! In case of name clashes define _ADT_SCOPE_NO_SIMPLIFIED to avoid them.
!##############################################################################

!---------------------------------------------------------
! link and bind macros for scalar symbols ...
!---------------------------------------------------------

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


!---------------------------------------------------------
! link and bind macros specialized for arrays ...
!---------------------------------------------------------

#   define _linkArray_as( scope, sym, id ) \
      _set_scopeArray_as( scope, sym, id )

#   define _linkArray( scope, sym ) \
      _linkArray_as( scope, sym, __sym2str__(sym) )

#   define _bindArray_as( scope, sym, id ) \
      _bind_scopeArray_as( scope, sym, id )

#   define _bindArray( scope, sym ) \
      _bindArray_as( scope, sym, __sym2str__(sym) )


#   define _declareHook( id ) \
      call declareCallback( __hookScope__, id )

#   define _callHook( hookId ) \
      call invokeCallback( __hookScope__, hookId )

#   define _callArgHook( hookId, arg ) \
      call invokeCallback( __hookScope__, hookId, c_loc(arg) )


#   define _removeSymbol( scope, sym ) \
      _remove_scopeSymbol( scope, __sym2str__(sym) )


!---------------------------------------------------------
! definine allocation macros these are needed to update
!   a scope symbol everytime it get [re-]allocated
!---------------------------------------------------------

# define _ALLOCATE_visible_as_st( scope, sym, shape, id, _st ) \
      select case (0); case default                           ;\
        _ALLOCATE_st( sym, shape, _st )                       ;\
        _linkArray_as( scope, sym, id )                       ;\
      end select

# define _checked_ALLOCATE_visible_as_st( scope, sym, shape, id, _st ) \
      select case (0); case default                                   ;\
        _checked_ALLOCATE_st( sym, shape, _st )                       ;\
        _linkArray_as( scope, sym, id )                               ;\
      end select

# define _assert_ALLOCATE_visible_as_st( scope, sym, shape, id, _st ) \
      select case (0); case default                                  ;\
        _assert_ALLOCATE_st( sym, shape, _st )                       ;\
        _linkArray_as( scope, sym, id )                              ;\
      end select

# define _assert_checked_ALLOCATE_visible_as_st( scope, sym, shape, id, _st ) \
      select case (0); case default                                          ;\
        _assert_checked_ALLOCATE_st( sym, shape, _st )                       ;\
        _linkArray_as( scope, sym, id )                                      ;\
      end select

# define _DEALLOCATE_visible_st( scope, sym, _st ) \
      select case (0); case default               ;\
        _DEALLOCATE_st( sym, _st )                ;\
        _removeSymbol( scope, sym )               ;\
      end select

# define _REALLOCATE_visible_st( scope, sym, shape, _st ) \
      select case (0); case default                      ;\
        _REALLOCATE_st( sym, shape, _st )                ;\
        _linkArray( scope, sym )                         ;\
      end select

# define _copied_REALLOCATE_visible_st( scope, sym, shape, pad, tmp, _st ) \
      select case (0); case default                                       ;\
        _copied_REALLOCATE_st( sym, shape, pad, tmp, _st )                ;\
        _linkArray( scope, sym )                                          ;\
      end select


# define _ALLOCATE_visible_st( scope, sym, shape, _st ) \
      _ALLOCATE_visible_as_st( scope, sym, shape, __sym2str__(sym), _st )

# define _checked_ALLOCATE_visible_st( scope, sym, shape, _st ) \
      _checked_ALLOCATE_visible_as_st( scope, sym, shape, __sym2str__(sym), _st )

# define _assert_ALLOCATE_visible_st( scope, sym, shape, _st ) \
      _assert_ALLOCATE_visible_as_st( scope, sym, shape, __sym2str__(sym), _st )

# define _assert_checked_ALLOCATE_visible_st( scope, sym, shape, _st ) \
      _assert_checked_ALLOCATE_visible_as_st( scope, sym, shape, __sym2str__(sym), _st )


!-------------------------------------------
! variants using status variable __istat__
!-------------------------------------------

# define _ALLOCATE_visible_as( scope, sym, shape, id ) \
     _ALLOCATE_visible_as_st( scope, sym, shape, id, __istat__ )

# define _checked_ALLOCATE_visible_as( scope, sym, shape, id ) \
     _checked_ALLOCATE_visible_as_st( scope, sym, shape, id, __istat__ )

# define _assert_ALLOCATE_visible_as( scope, sym, shape, id ) \
     _assert_ALLOCATE_visible_as_st( scope, sym, shape, id, __istat__ )

# define _assert_checked_ALLOCATE_visible_as( scope, sym, shape, id ) \
     _assert_checked_ALLOCATE_visible_as_st( scope, sym, shape, id, __istat__ )

# define _DEALLOCATE_visible( scope, sym ) \
     _DEALLOCATE_visible_st( scope, sym, __istat__ )

# define _REALLOCATE_visible( scope, sym, shape ) \
     _REALLOCATE_visible_st( scope, sym, shape, __istat__ )

# define _copied_REALLOCATE_visible( scope, sym, shape, pad, tmp ) \
     _copied_REALLOCATE_visible_st( scope, sym, shape, pad, tmp, __istat__ )

# define _ALLOCATE_visible( scope, sym, shape ) \
     _ALLOCATE_visible_st( scope, sym, shape, __istat__ )

# define _checked_ALLOCATE_visible( scope, sym, shape ) \
     _checked_ALLOCATE_visible_st( scope, sym, shape, __istat__ )

# define _assert_ALLOCATE_visible( scope, sym, shape ) \
     _assert_ALLOCATE_visible_st( scope, sym, shape, __istat__ )

# define _assert_checked_ALLOCATE_visible( scope, sym, shape ) \
     _assert_checked_ALLOCATE_visible_st( scope, sym, shape, __istat__ )

# endif
 ! ^^ _ADT_SCOPE_NO_SIMPLIFIED


# if !defined _ADT_SCOPE_NO_RETRIEVE
!##############################################################################
! The following definitions simplify the data retrieval from adt scopes
! In case of name clashes define _ADT_SCOPE_NO_RETRIEVE to avoid them.
!##############################################################################

! define some exception messages ...
#   define __no_item(id) \
      "can't locate item named "//id

#   define __ill_var_ref(id) \
      "type mismatch at getting variable reference of "//id

#   define __ill_val_ref(id) \
      "unable to take value reference of "//id

#   define __type_mismatch_what(what, id) \
      "type mismatch at setting "//what//" "//id

#   define __type_mismatch_routine(id) \
      id//" does not refer to routine"


! The following definitions facilitate getting type-safe references of
!   variables and values provided by the given ATHLET-scope.
! In these macros the type check is performed by dynamic_cast() which
!   signals a type mismatch by returning .false.
! In such case an exception is thrown.
!
! In ATHLET there are TWO types of data items:
!   * global module data objects (arrays, variables, etc.)
!   * temporary values stored within the scope and that might get
!       updated during the simulation
!
! Both types can be handled the same way by using pointers BUT currently
!   the way of retrieving such pointer is a bit different.
! That's why there are two versions: _refVar and _refVal


! _refVar: reference a linked data object, e.g. a variable
# define _refVar( scope, id, ptr ) \
    if (.not. dynamic_cast( ptr, getRef( scope, id ) )) \
      call throw( TypeError, __ill_var_ref(id) )


! _refVal: reference a value, stored within the scope.
# define _refVal( scope, id, ptr ) \
    if (.not. dynamic_cast( ptr, getItem( scope, id ) )) \
      call throw( TypeError, __ill_val_ref(id) )


! _refVal_default: reference a temporary scope value, if it does not yet
!                  exist it gets created and initialized to the provided
!                  default value.
# define _refVal_default( scope, id, ptr, defaultVal ) \
    if (.not. dynamic_cast( ptr, setDefault( scope, id, Item_of(defaultVal) ) )) \
      call throw( TypeError, __type_mismatch_what('default value', id) )


! _refVar_check: reference linked data object if exists
# define _refVar_check( scope, id, ptr ) \
    if (hasKey( scope, id )) then       ;\
      _refVar( scope, id, ptr )         ;\
    end if

! _refProc: connect given routine pointer <ptr> to a procedure linked in <scope>.
!           Note that this macro assumes TWO things about <ptr>:
!             - it is named like the procedure to get from scope
!             - it should be associated to the correct interface
# define _refProc( scope, ptr ) \
    call c_f_procpointer( getProcedure( scope, _strip(_str(ptr)) ), ptr )


!
! non-raising-version of _ref#-macros
!
! _try_refVar: try to reference a linked data object
!              return .true. on success, .false. otherwise
# define _try_refVar( scope, id, ptr ) \
    dynamic_cast( ptr, getRef( scope, id, .false. ) )


! _try_refVal: try to reference a value, stored within the scope.
!              return .true. on success, .false. otherwise
# define _try_refVal( scope, id, ptr ) \
    dynamic_cast( ptr, getItem( scope, id, .false. ) )


! _refProc_null: connect given routine pointer <ptr> to a procedure linked in <scope>.
!               if it fails, ptr is set to c_null_funptr
# define _refProc_null( scope, ptr ) \
    call c_f_procpointer( getProcedure( scope, _strip(_str(ptr)), .false. ), ptr )


! deprecated - just for compatibility
# define _getService  _refProc

# endif
 ! ^^ _ADT_SCOPE_NO_RETRIEVE
#endif
! ^^ __ADT_SCOPE_FPP

