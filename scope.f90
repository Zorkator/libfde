
#include "adt/itfUtil.fpp"

module adt_scope
  use adt_hashmap
  implicit none
  private

  integer, parameter :: hook_disabled   = -2
  integer, parameter :: hook_undeclared = -1
  integer, parameter :: hook_not_set    =  0
  integer, parameter :: hook_called     =  1

  public :: newScope, getScope
  public :: declareCallback, setCallback, tryCallback, invokeCallback
  public :: hook_disabled, hook_undeclared, hook_not_set, hook_called

  interface newScope
    function scope_create_() result(scope)
      import HashMap_t
      type(HashMap_t), pointer :: scope
    end function
  end interface

  interface getScope
    function scope_get_subscope_( parent, id ) result(scope)
      import HashMap_t
      type(HashMap_t)          :: parent
      character(len=*)         :: id
      type(HashMap_t), pointer :: scope
    end function

    function scope_get_subscope_seq_( parent, id1, id2, id3, id4, id5, id6, id7, id8, id9 ) result(scope)
      import HashMap_t
      type(HashMap_t)            :: parent
      character(len=*)           :: id1, id2
      character(len=*), optional :: id3, id4, id5, id6, id7, id8, id9
      type(HashMap_t),   pointer :: scope
    end function
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

end module


