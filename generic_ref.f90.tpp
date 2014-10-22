
#include "adt/ref_status.fpp"

module generic_ref
  use base_string
  use type_info
  use iso_c_binding
  implicit none
  private


  type, public :: GenericRef_t
    private
    _RefStatus                :: refstat = _ref_HardLent
    type(BaseString_t)        :: ref_str
    type(TypeInfo_t), pointer :: typeInfo => null()
  end type

  !_TypeGen_declare_RefType( public, ref, type(GenericRef_t), scalar, \
  !     initProc   = gr_initialize, \
  !     assignProc = gr_assign_gr,  \
  !     deleteProc = gr_delete,     \
  !     cloneMode  = _type )


  type, public :: GenericRef_Encoding_t
    integer, pointer :: ptr
  end type


  type, private :: RefControl_t
    private
    integer*1 :: val = 0
  end type


  type(GenericRef_t), parameter :: permanent_ref     = GenericRef_t( _ref_HardLent, permanent_string, null() )
  type(GenericRef_t), parameter :: temporary_ref     = GenericRef_t( _ref_WeakLent, temporary_string, null() )
  type(RefControl_t), parameter :: release_reference = RefControl_t(0)
  type(RefControl_t), parameter :: accept_reference  = RefControl_t(1)


  ! interface definitions

  interface rank        ; module procedure gr_rank                          ; end interface
  interface shape       ; module procedure gr_shape                         ; end interface
  interface clone       ; module procedure gr_clone                         ; end interface
  interface cptr        ; module procedure gr_cptr                          ; end interface
  interface delete      ; module procedure gr_delete                        ; end interface
  interface free        ; module procedure gr_free                          ; end interface
  interface dynamic_type; module procedure gr_dynamic_type                  ; end interface
  interface ref_control ; module procedure gr_ref_control                   ; end interface

  ! assignment and operators

  interface assign        ; module procedure gr_assign_gr, gr_assign_encoding ; end interface
  interface assignment(=) ; module procedure gr_assign_gr, gr_assign_encoding ; end interface

  ! declare public interfaces 

  public :: assign, assignment(=)
  public :: gr_get_TypeReference  !< needed by generated code
  public :: rank
  public :: shape
  public :: clone
  public :: cptr
  public :: delete
  public :: free
  public :: dynamic_type
  public :: permanent_ref, temporary_ref
  public :: release_reference, accept_reference, ref_control

  ! declare type_info necessities public

  public :: TypeInfo_t, TypeInfo_ptr_t, init_TypeInfo, type_void, void_t 

!-----------------
  contains
!-----------------

  !_TypeGen_implementAll()


  subroutine gr_initialize( self, has_proto, proto )
    type(GenericRef_t) :: self
    integer            :: has_proto
    type(GenericRef_t) :: proto
    
    if (has_proto /= 0) then;
      _ref_init( self%refstat, _ref_hardness(proto%refstat) )
      call basestring_init_by_proto( self%ref_str, 1, proto%ref_str )
    else;
      self%refstat = _ref_HardLent
      call basestring_init_by_proto( self%ref_str, 0, self%ref_str )
    end if
    self%typeInfo => null()
  end subroutine


  subroutine gr_assign_gr( lhs, rhs )
    type(GenericRef_t), intent(inout) :: lhs
    type(GenericRef_t),    intent(in) :: rhs

    if (.not. associated(lhs%ref_str%ptr, rhs%ref_str%ptr)) then
      call gr_free( lhs )
      call basestring_assign_bs( lhs%ref_str, rhs%ref_str )
      lhs%typeInfo => rhs%typeInfo

      if (_ref_isWeakMine( rhs%refstat )) &
        _ref_setMine( lhs%refstat, 1 )
    end if
  end subroutine


  subroutine gr_assign_encoding( lhs, rhs )
    type(GenericRef_t),               intent(inout) :: lhs
    type(GenericRef_Encoding_t), target, intent(in) :: rhs(:)
    integer*4,                            parameter :: size_typeInfo = storage_size(TypeInfo_ptr_t(null())) / 8
    integer*4,                            parameter :: size_encoding = storage_size(GenericRef_Encoding_t(null())) / 8
    character(len=1), dimension(:),         pointer :: stream
    type(TypeInfo_ptr_t),                   pointer :: typeInfo
    type(c_ptr)                                     :: encoding
    
    call gr_free( lhs )
    encoding = c_loc(rhs(1))
    call c_f_pointer( encoding, typeInfo )
    call c_f_pointer( encoding, stream, (/ size(rhs) * size_encoding /) )
    call basestring_assign_buf( lhs%ref_str, stream(size_typeInfo + 1:) )
    lhs%typeInfo => typeInfo%ptr
  end subroutine


  function gr_get_TypeReference( self ) result(res)
    type(GenericRef_t), intent(in) :: self
    type(c_ptr)                    :: res
    res = basestring_cptr( self%ref_str )
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

    call basestring_set_attribute( res%ref_str, attribute_volatile )
    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%cloneRefProc )) then
        call self%typeInfo%cloneRefProc( res, self )
        res%refstat = _ref_WeakMine
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


  recursive &
  subroutine gr_delete( self )
    type(GenericRef_t) :: self

    call gr_free( self )
    call basestring_delete( self%ref_str )
    self%typeInfo => null()
  end subroutine


  recursive &
  subroutine gr_free( self )
    type(GenericRef_t)    :: self
    type(void_t), pointer :: wrap
    type(c_ptr)           :: cptr

    if (_ref_isMine( self%refstat )) then
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
      _ref_setMine( self%refstat, 0 )
    end if
  end subroutine


  function gr_dynamic_type( self ) result(res)
    type(GenericRef_t), intent(in) :: self
    type(TypeInfo_t),      pointer :: res
    if (associated( self%typeInfo )) then; res => self%typeInfo
                                     else; res => type_void
    end if
  end function

  
  subroutine gr_ref_control( self, ctrl )
    type(GenericRef_t), intent(inout) :: self
    type(RefControl_t),    intent(in) :: ctrl
    _ref_setMine( self%refstat, ctrl%val )
  end subroutine

end module

