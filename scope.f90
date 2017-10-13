
#include "adt/itfUtil.fpp"

module adt_scope
  use adt_hashmap
  use adt_item
  use adt_ref
  use iso_c_binding
  implicit none
  private

  integer, parameter :: hook_disabled   = -2
  integer, parameter :: hook_undeclared = -1
  integer, parameter :: hook_not_set    =  0
  integer, parameter :: hook_called     =  1

  integer, parameter :: event_undeclared = -1
  integer, parameter :: event_not_set    =  0
  integer, parameter :: event_connected  =  1

  type(HashMap_t), pointer :: processScope_

  public :: newScope, getScope, getItem, getRef, setProcedure, getProcedure
  public :: declareCallback, setCallback, tryCallback, invokeCallback
  public :: hook_disabled, hook_undeclared, hook_not_set, hook_called
  public :: declareEvent, connect, disconnect, emit
  public :: event_undeclared, event_not_set, event_connected

  interface newScope
    function scope_create_() result(scope)
      import HashMap_t
      type(HashMap_t), pointer :: scope
    end function
  end interface

  interface getScope
    module procedure scope_get_processscope_
    module procedure scope_get_processscope_seq_

    function scope_get_subscope_seq_( parent, id1, id2, id3, id4, id5, id6, id7, id8, id9 ) result(scope)
      import HashMap_t
      type(HashMap_t)            :: parent
      character(len=*)           :: id1
      character(len=*), optional :: id2, id3, id4, id5, id6, id7, id8, id9
      type(HashMap_t),   pointer :: scope
    end function
  end interface

  interface getItem
    function scope_get_item_( scope, id, raise ) result(res)
      import HashMap_t, Item_t
      type(HashMap_t)        :: scope
      character(len=*)       :: id
      logical,      optional :: raise
      type(Item_t), pointer  :: res
    end function
  end interface

  interface getRef
    function scope_get_ref_( scope, id, raise ) result(res)
      import HashMap_t, Ref_t
      type(HashMap_t)        :: scope
      character(len=*)       :: id
      logical,      optional :: raise
      type(Ref_t),  pointer  :: res
    end function
  end interface

  interface setProcedure
    subroutine scope_set_procedure_( scope, id, proc_ )
      import HashMap_t
      type(HashMap_t)   :: scope
      character(len=*)  :: id
      procedure()       :: proc_
    end subroutine
  end interface

  interface getProcedure
    function scope_get_procedure_( scope, id, raise ) result(res)
      import HashMap_t, c_funptr
      type(HashMap_t)   :: scope
      character(len=*)  :: id
      logical, optional :: raise
      type(c_funptr)    :: res
    end function
  end interface

  interface declareEvent
    subroutine scope_declare_event_c( scope, ident )
      import HashMap_t
      type(HashMap_t)       :: scope
      character(len=*)      :: ident
    end subroutine
  end interface

  interface connect
    integer &
    function scope_connect_event_c( self, ident, proc ) result(res)
      import HashMap_t
      type(HashMap_t)       :: self
      character(len=*)      :: ident
      procedure(), optional :: proc
    end function
  end interface

  interface disconnect
    integer &
    function scope_disconnect_event_c( self, ident, proc ) result(res)
      import HashMap_t
      type(HashMap_t)       :: self
      character(len=*)      :: ident
      procedure(), optional :: proc
    end function
  end interface

  interface emit
    subroutine scope_emit_event_c( self, ident, arg )
      import HashMap_t, c_ptr
      type(HashMap_t)       :: self
      character(len=*)      :: ident
      type(c_ptr), optional :: arg
    end subroutine
  end interface

  interface declareCallback
    subroutine scope_declare_callback_c( scope, ident )
      import HashMap_t
      type(HashMap_t)       :: scope
      character(len=*)      :: ident
    end subroutine
  end interface

  interface setCallback
    integer &
    function scope_set_callback_c( scope, ident, proc )
      import HashMap_t
      type(HashMap_t)       :: scope
      character(len=*)      :: ident
      procedure(), optional :: proc
    end function
  end interface

  interface tryCallback
    integer &
    function scope_try_callback_c( self, ident, argScope ) result(code)
      ! code  : hook_undeclared | hook_not_set | hook_called
      import HashMap_t
      type(HashMap_t)           :: self
      character(len=*)          :: ident
      type(HashMap_t), optional :: argScope
    end function
  end interface

  interface invokeCallback
    subroutine scope_invoke_callback_c( self, ident, argScope )
      import HashMap_t
      type(HashMap_t)           :: self
      character(len=*)          :: ident
      type(HashMap_t), optional :: argScope
    end subroutine
  end interface

  contains

!_PROC_EXPORT(scope_get_processscope_)
  function scope_get_processscope_() result(res)
    type(HashMap_t), pointer :: res
    if (.not. associated( processScope_ )) then
      allocate( processScope_ )
      call initialize( processScope_ )
    end if
    res => processScope_
  end function

!_PROC_EXPORT(scope_get_processscope_seq_)
  function scope_get_processscope_seq_( id1, id2, id3, id4, id5, id6, id7, id8, id9 ) result(scope)
    character(len=*)           :: id1
    character(len=*), optional :: id2, id3, id4, id5, id6, id7, id8, id9
    type(HashMap_t),   pointer :: scope
    scope => getScope( getScope(), id1, id2, id3, id4, id5, id6, id7, id8, id9 )
  end function

end module


