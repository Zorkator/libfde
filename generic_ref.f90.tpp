
module generic_ref
  use base_string
  use type_info
  use iso_c_binding
  implicit none
  private


  type, public :: GenericRef_t
    private
    type(BaseString_t)        :: ref_str
    type(TypeInfo_t), pointer :: typeInfo => null()
  end type

  type, public :: GenericRef_Encoding_t
    integer, pointer :: ptr
  end type

  ! interface definitions

  interface assignment(=)
    module procedure gr_assign_gr, gr_assign_encoding
  end interface

  interface rank        ; module procedure gr_rank        ; end interface
  interface shape       ; module procedure gr_shape       ; end interface
  interface clone       ; module procedure gr_clone       ; end interface
  interface cptr        ; module procedure gr_cptr        ; end interface
  interface delete      ; module procedure gr_delete      ; end interface
  interface free        ; module procedure gr_free        ; end interface
  interface dynamic_type; module procedure gr_dynamic_type; end interface

  ! declare public interfaces 

  public :: assignment(=)
  public :: gr_get_TypeReference  !< needed by generated code
  public :: rank
  public :: shape
  public :: clone
  public :: cptr
  public :: delete
  public :: free
  public :: dynamic_type

  ! declare type_info necessities public

  public :: TypeInfo_t, TypeInfo_ptr_t, init_TypeInfo, type_void, void_t 

  !_TypeGen_declare_RefType( public, ref, type(GenericRef_t), scalar, \
  !     initProc   = gr_initialize, \
  !     assignProc = gr_assign_gr,  \
  !     deleteProc = gr_delete,     \
  !     cloneMode  = _type )


!-----------------
  contains
!-----------------

  !_TypeGen_implementAll()


  subroutine gr_initialize( self, hardness )
    type(GenericRef_t) :: self
    integer            :: hardness
    call bs_init( self%ref_str, hardness )
    self%typeInfo => null()
  end subroutine


  subroutine gr_assign_gr( lhs, rhs )
    type(GenericRef_t), intent(inout) :: lhs
    type(GenericRef_t),    intent(in) :: rhs

    call bs_assign_bs( lhs%ref_str, rhs%ref_str )
    lhs%typeInfo => rhs%typeInfo
  end subroutine


  subroutine gr_assign_encoding( lhs, rhs )
    type(GenericRef_t),                             intent(inout) :: lhs
    type(GenericRef_Encoding_t), dimension(:), target, intent(in) :: rhs
    character(len=1), dimension(:),                       pointer :: fptr
    type(TypeInfo_ptr_t)                                          :: tiWrap
    type(TypeInfo_ptr_t),                                 pointer :: tiPtr
    integer*4,                                          parameter :: ref_idx = storage_size(tiWrap)/8 + 1
    
    call c_f_pointer( c_loc(rhs(1)), fptr, (/ size(rhs) * storage_size(GenericRef_Encoding_t(null()))/8 /) )
    call c_f_pointer( c_loc(rhs(1)), tiPtr )
    call bs_assign_buf( lhs%ref_str, fptr(ref_idx:) )
    lhs%typeInfo => tiPtr%ptr
  end subroutine


  function gr_get_TypeReference( self ) result(res)
    type(GenericRef_t), intent(in) :: self
    type(c_ptr)                    :: res
    res = bs_cptr( self%ref_str )
  end function


  pure function gr_rank( self ) result(res)
    type(GenericRef_t), intent(in) :: self
    integer                        :: res

    if (associated( self%typeInfo )) then
      res = self%typeInfo%rank
    else
      res = 0
    end if
  end function


  pure function gr_shape( self ) result(res)
    type(GenericRef_t), intent(in) :: self
    integer                        :: res(rank(self))

    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%shapeProc )) then
        call self%typeInfo%shapeProc( self, res, self%typeInfo%rank )
        return
      end if
    end if
    res = 0
  end function


  function gr_clone( self ) result(res)
    type(GenericRef_t), intent(in) :: self
    type(GenericRef_t)             :: res

    call bs_set_attribute( res%ref_str, attrib_volatile )
    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%cloneRefProc )) then
        call self%typeInfo%cloneRefProc( res, self )
        return
      end if
    end if
    res = self
  end function


  function gr_cptr( self ) result(res)
    type(GenericRef_t), intent(in) :: self
    type(c_ptr)                    :: res
    type(void_t),          pointer :: wrap

    res = gr_get_TypeReference(self)
    if (c_associated(res)) then
      call c_f_pointer( res, wrap )
      res = c_loc(wrap%ptr)
    end if
  end function


  subroutine gr_delete( self )
    type(GenericRef_t) :: self

    call bs_delete( self%ref_str )
    self%typeInfo => null()
  end subroutine


  subroutine gr_free( self )
    type(GenericRef_t)    :: self
    type(void_t), pointer :: wrap
    type(c_ptr)           :: cptr

    cptr = gr_get_TypeReference(self)
    if (c_associated( cptr )) then
      call c_f_pointer( cptr, wrap )

      if (associated( wrap%ptr )) then
        if (associated( self%typeInfo )) then
          if (associated( self%typeInfo%deleteProc )) &
            call self%typeInfo%deleteProc( wrap%ptr )
        end if
        deallocate( wrap%ptr )
      end if
    end if
    call delete( self )
  end subroutine


  function gr_dynamic_type( self ) result(res)
    type(GenericRef_t), intent(in) :: self
    type(TypeInfo_t),      pointer :: res
    if (associated( self%typeInfo )) then; res => self%typeInfo
                                     else; res => type_void
    end if
  end function

end module

