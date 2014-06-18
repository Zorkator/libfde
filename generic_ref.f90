
module generic_ref
  use dynamic_string
  use type_info
  use iso_c_binding
  implicit none
  private

  type, public :: GenericRef
    private
    type (DynamicString)     :: ref
    type (TypeInfo), pointer :: typeInfo => null()
  end type


  ! declare public interfaces 

  public :: assignment(=)
  public :: gr_set_TypeReference
  public :: gr_get_TypeReference
  public :: delete
  !public :: free


  ! interface definitions

  interface assignment(=)
    module procedure gr_assign_gr
  end interface

  interface delete; module procedure gr_delete; end interface
  !interface free  ; module procedure gr_free  ; end interface


  type, public :: voidRef ; integer, pointer :: ptr; end type

!-----------------
  contains
!-----------------


  subroutine gr_assign_gr( lhs, rhs )
    type (GenericRef), intent(inout) :: lhs
    type (GenericRef),    intent(in) :: rhs

    lhs%ref      =  rhs%ref
    lhs%typeInfo => rhs%typeInfo
  end subroutine


  function gr_set_TypeReference( self, cptr, bits, ti ) result(needInit)
    type (GenericRef), intent(inout) :: self
    type (c_ptr),         intent(in) :: cptr
    integer*4,            intent(in) :: bits
    type (TypeInfo),          target :: ti
    character(len=bits/8),   pointer :: ptr
    logical                          :: needInit

    call c_f_pointer( cptr, ptr )
    self%ref = VolatileString()
    self%ref = ptr
    needInit = .not. ti%initialized
    self%typeInfo => ti
  end function


  function gr_get_TypeReference( self ) result(res)
    type (GenericRef), intent(in) :: self
    type (c_ptr)                  :: res
    res = cptr(self%ref)
  end function


  subroutine gr_delete( self )
    type (GenericRef) :: self

    call delete( self%ref )
    self%typeInfo => null()
  end subroutine


  subroutine gr_free( self )
    type (GenericRef)       :: self
    type (voidRef), pointer :: wrap

    if (len(self%ref) > 0) then
      call c_f_pointer( gr_get_TypeReference(self), wrap )

      if (associated(wrap%ptr)) then
        if (associated(self%typeInfo)) then
        end if
      end if
    end if
    call delete( self )
  end subroutine

end module


module encoders
  use generic_ref
  use type_info
  implicit none

  type, public :: Int; integer*4, pointer :: ptr; end type
  type (TypeInfo), target :: TypeInfo_Int
  interface GenericRef; module procedure GenericRef_encode_Int; end interface
  interface IntPtr;     module procedure GenericRef_decode_Int; end interface

  contains

  function GenericRef_encode_Int( val ) result(res)
    use iso_c_binding
    integer*4, target, intent(in) :: val
    type (GenericRef)             :: res
    type (Int),            target :: wrap

    wrap%ptr => val
    if (gr_set_TypeReference( res, c_loc(wrap), storage_size(wrap), TypeInfo_Int )) &
      call TypeInfo_init( TypeInfo_Int, "Int", "integer*4", storage_size(val)/8, size(shape(val)) )
  end function


  function GenericRef_decode_Int( val ) result(res)
    use iso_c_binding
    type (GenericRef), intent(in) :: val
    integer*4,            pointer :: res
    type (Int),           pointer :: wrap
    
    call c_f_pointer( gr_get_TypeReference(val), wrap )
    res => wrap%ptr
  end function

end module


!##################################################################################################
#ifdef TEST

program testinger
  use generic_ref
  use encoders
  implicit none

  integer*4         :: val
  integer*4, pointer:: ptr => null()
  type (GenericRef) :: ref

  ref = GenericRef(val)
  ptr => IntPtr(ref)
  ptr = 42

  call delete( ref )

end

#endif

