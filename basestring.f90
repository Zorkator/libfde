
#include "adt/ref_status.fpp"

module adt_basestring
  use iso_c_binding
  implicit none
  private
  
  type, public :: BaseString_t
    _RefStatus                              :: refstat = _ref_HardLent
    integer*4                               :: len     = 0
    character(len=1), dimension(:), pointer :: ptr     => null()
  end type

  type(BaseString_t), parameter :: temporary_string    = BaseString_t( _ref_WeakLent, 0, null() )
  type(BaseString_t), parameter :: permanent_string    = BaseString_t( _ref_WeakLent, 0, null() )
  integer(kind=1),    parameter :: attribute_volatile  = 0
  integer(kind=1),    parameter :: attribute_permanent = 1

  ! interface definitions

  interface
    subroutine basestring_init_by_proto( bs, has_proto, proto )
      import BaseString_t
      type(BaseString_t), intent(inout) :: bs
      integer,            intent(in)    :: has_proto
      type(BaseString_t), intent(in)    :: proto
    end subroutine

    subroutine basestring_init_by_charString( bs, cs )
      import BaseString_t
      type(BaseString_t), intent(inout) :: bs
      character(len=*),   intent(in)    :: cs
    end subroutine

    subroutine basestring_init_by_buf( bs, buf )
      import BaseString_t
      type(BaseString_t),             intent(inout) :: bs
      character(len=1), dimension(:), intent(in)    :: buf
    end subroutine

    subroutine basestring_set_attribute( bs, attr )
      import BaseString_t
      type(BaseString_t), intent(inout) :: bs
      integer(kind=1),    intent(in)    :: attr
    end subroutine

    subroutine basestring_release_weak( bs )
      import BaseString_t
      type(BaseString_t) :: bs
    end subroutine

    subroutine basestring_delete( bs )
      import BaseString_t
      type(BaseString_t), intent(inout) :: bs
    end subroutine

    function basestring_ptr( bs ) result(res)
      import BaseString_t
      type(BaseString_t), intent(in) :: bs
      character(len=bs%len), pointer :: res
    end function

    function basestring_cptr( bs ) result(res)
      import BaseString_t, c_ptr
      type(BaseString_t), intent(in) :: bs
      type(c_ptr)                    :: res
    end function

    pure &
    function basestring_len_ref( bs ) result(res)
      import BaseString_t
      type(BaseString_t), intent(in) :: bs
      integer                        :: res
    end function

    subroutine basestring_assign_charString( bs, cs )
      import BaseString_t
      type(BaseString_t), intent(inout) :: bs
      character(len=*),   intent(in)    :: cs
    end subroutine

    subroutine basestring_assign_buf( lhs, rhs )
      import BaseString_t
      type(BaseString_t),             intent(inout) :: lhs
      character(len=1), dimension(:), intent(in)    :: rhs
    end subroutine

    subroutine basestring_assign_basestring( lhs, rhs )
      import BaseString_t
      type(BaseString_t), intent(inout) :: lhs
      type(BaseString_t),    intent(in) :: rhs
    end subroutine
  end interface

  ! interface visibility

  public :: basestring_init_by_proto
  public :: basestring_init_by_charString
  public :: basestring_init_by_buf
  public :: basestring_set_attribute
  public :: basestring_release_weak
  public :: basestring_delete
  public :: basestring_ptr
  public :: basestring_cptr
  public :: basestring_len_ref
  public :: basestring_assign_charString
  public :: basestring_assign_buf
  public :: basestring_assign_basestring

  public :: temporary_string, permanent_string
  public :: attribute_volatile, attribute_permanent

end module

