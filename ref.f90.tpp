
#include "adt/ref_status.fpp"
#include "adt/itfUtil.fpp"

module adt_ref
  use adt_basestring
  use adt_typeinfo
  use iso_c_binding
  implicit none
  private


  type, public :: Ref_t
    private
    _RefStatus                :: refstat = _ref_HardLent
    type(BaseString_t)        :: ref_str
    type(TypeInfo_t), pointer :: typeInfo => null()
  end type


  !_TypeGen_declare_RefType( public, ref, type(Ref_t), scalar, \
  !     initProc   = ref_init_by_proto, \
  !     assignProc = ref_assign_ref_c, \
  !     deleteProc = ref_delete_c,     \
  !     cloneMode  = _type )


  type, public :: RefEncoding_t
    integer, pointer :: ptr
  end type


  type(Ref_t), parameter :: permanent_ref = Ref_t( _ref_HardLent, permanent_string, null() )
  type(Ref_t), parameter :: temporary_ref = Ref_t( _ref_WeakLent, temporary_string, null() )


  ! interface definitions

  interface
    subroutine ref_init_by_proto( self, has_proto, proto )
      import Ref_t
      type(Ref_t) :: self, proto
      integer     :: has_proto
    end subroutine

    function ref_get_typereference( self ) result(res)
      import Ref_t, c_ptr
      type(Ref_t), intent(in) :: self
      type(c_ptr)             :: res
    end function
  end interface

  interface rank
    pure &
    function ref_rank( self ) result(res)
      import Ref_t
      type(Ref_t), intent(in) :: self
      integer                 :: res
    end function
  end interface

  interface shape
    pure &
    function ref_shape( self ) result(res)
      import Ref_t
      type(Ref_t), intent(in) :: self
      integer                 :: res(ref_rank(self))
    end function
  end interface

  interface clone
    function ref_clone( self ) result(res)
      import Ref_t
      type(Ref_t), intent(in) :: self
      type(Ref_t)             :: res
    end function
  end interface

  interface cptr
    function ref_cptr( self ) result(res)
      import Ref_t, c_ptr
      type(Ref_t), intent(in) :: self
      type(c_ptr)             :: res
    end function
  end interface

  interface delete
    recursive &
    subroutine ref_delete_c( self )
      import Ref_t
      type(Ref_t) :: self
    end subroutine
  end interface

  interface free
    recursive &
    subroutine ref_free( self )
      import Ref_t
      type(Ref_t)           :: self
    end subroutine
  end interface

  interface dynamic_type
    function ref_dynamic_type( self ) result(res)
      import Ref_t, TypeInfo_t
      type(Ref_t),   intent(in) :: self
      type(TypeInfo_t), pointer :: res
    end function
  end interface

  interface bind
    subroutine ref_bind( self, do_bind )
      import Ref_t
      type(Ref_t), intent(inout) :: self
      logical                    :: do_bind
    end subroutine
  end interface

  ! assignment and operators

  interface assign
    subroutine ref_assign_ref_c( lhs, rhs )
      import Ref_t
      type(Ref_t), intent(inout) :: lhs
      type(Ref_t),    intent(in) :: rhs
    end subroutine

    subroutine ref_assign_encoding( lhs, rhs )
      import Ref_t, RefEncoding_t
      type(Ref_t),              intent(inout) :: lhs
      type(RefEncoding_t), target, intent(in) :: rhs(:)
    end subroutine
  end interface

  interface assignment(=)
    module procedure ref_assign_ref_private, ref_assign_encoding_private
  end interface


  ! declare public interfaces 

  public :: assign, assignment(=)
  public :: ref_get_typereference  !< needed by generated code
  public :: rank
  public :: shape
  public :: clone
  public :: cptr
  public :: delete
  public :: free
  public :: dynamic_type
  public :: permanent_ref, temporary_ref
  public :: bind

  ! declare TypeInfo necessities public

  public :: TypeInfo_t, TypeInfo_ptr_t, type_void, void_t, typeInfo_init

!-----------------
  contains
!-----------------

  !_TypeGen_implementAll()

!_PROC_EXPORT(ref_assign_ref_private)
!_ARG_REFERENCE2(lhs, rhs)
  subroutine ref_assign_ref_private( lhs, rhs )
    type(Ref_t), intent(inout) :: lhs
    type(Ref_t),    intent(in) :: rhs
    call assign( lhs, rhs )
  end subroutine

!_PROC_EXPORT(ref_assign_encoding_private)
!_ARG_REFERENCE2(lhs, rhs)
  subroutine ref_assign_encoding_private( lhs, rhs )
    type(Ref_t),              intent(inout) :: lhs
    type(RefEncoding_t), target, intent(in) :: rhs(:)
    call assign( lhs, rhs )
  end subroutine

end module

