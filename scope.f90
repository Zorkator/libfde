
module adt_scope
  use adt_hashmap
  implicit none
  private

  public :: newScope, getScope, setCallback
  public :: print_scope

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

  interface setCallback
    integer &
    function scope_set_callback_c( scope, ident, proc )
      import HashMap_t
      type(HashMap_t)  :: scope
      character(len=*) :: ident
      external         :: proc
    end function
  end interface

  interface
    recursive &
    subroutine print_scope( scope, level )
      import HashMap_t
      type(HashMap_t) :: scope
      integer         :: level
    end subroutine
  end interface

  contains

end module


