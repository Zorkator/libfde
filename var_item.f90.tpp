
#include "adt/var_item.fpp"

module var_item
  use iso_c_binding
  use generic_ref
  use dynamic_string
  use base_types
  implicit none
  private

! declare dummy variables - used to determine each type's storage_size ...
# define _varitem_type_(typeId, baseType)   baseType :: _paste(typeId,_var);
    _TableOf_varitem_types_
# undef _varitem_type_


# define _varitem_type_(typeId, baseType) \
    , storage_size(_paste(typeId,_var))

# define _chunkType   integer*8
  _chunkType, parameter :: chunk_var = 0
  integer*4,  parameter :: maxBytes  = max(0 _TableOf_varitem_types_)/8
  integer*4,  parameter :: chunkSize = storage_size(chunk_var)/8
  integer*4,  parameter :: numChunks = ceiling( maxBytes / real(chunkSize) )
# undef _varitem_type_


  type, public :: VarItem_t
    private
    _chunkType                :: data(numChunks) = 0
    type(TypeInfo_t), pointer :: typeInfo        => null()
  end type

  ! declare VarItemOf(<type>) interface ...
  ! NOTE: It's named VarItemOf to avoid ambiguity with VarItem-interface for type deref.
# define _varitem_type_(typeId, baseType) \
    , _paste(vi_from_,typeId)

  interface VarItemOf
    module procedure vi_from_vi, vi_from_charString &
                     _TableOf_varitem_types_
  end interface
# undef _varitem_type_


  ! declare public <typeId>-interfaces for getting type pointers
# define _varitem_type_(typeId, baseType) \
    interface typeId; module procedure _paste(vi_get_,typeId); end interface; \
    public :: typeId;
    _TableOf_varitem_types_
# undef _varitem_type_


  ! declare assignment interface
# define _varitem_type_(typeId, baseType) \
    , _paste(typeId,_assign_vi), _paste(vi_assign_,typeId)

  interface assignment(=)
    module procedure vi_assign_vi, vi_assign_charString &
                     _TableOf_varitem_types_
  end interface
# undef _varitem_type_

  ! declare interfaces public

    interface is_valid    ; module procedure vi_is_valid    ; end interface
    interface dynamic_type; module procedure vi_dynamic_type; end interface
    interface delete      ; module procedure vi_delete      ; end interface

    public :: VarItemOf
    public :: is_valid
    public :: dynamic_type
    public :: delete
    public :: assignment (=)


  ! declare public typecheck interfaces ...
# define _varitem_type_(typeId, baseType) \
    interface _paste(is_,typeId); module procedure _paste(vi_is_,typeId); end interface; \
    public :: _paste(is_,typeId);

    _TableOf_varitem_types_
# undef _varitem_type_

  !_TypeReference_declare( public, VarItem, type(VarItem_t), scalar, \
  !     initProc   = vi_initialize, \
  !     assignProc = vi_assign_vi,  \
  !     deleteProc = vi_delete,     \
  !     cloneMode  = _type )

  
!-----------------
  contains
!-----------------

  !_TypeReference_implementAll()
  
  
  subroutine vi_initialize( self )
    type(VarItem_t) :: self
    self%data     = 0
    self%typeInfo => null()
  end subroutine


! implement constructor routines

# define _implementConstructor_(typeId, baseType)          \
    function _paste(vi_from_,typeId)( val ) result(res)   ;\
      baseType,    intent(in) :: val                      ;\
      baseType,       pointer :: ptr                      ;\
      type(VarItem_t), target :: res                      ;\
      call vi_reshape( res, static_type(val), 0 )         ;\
      call c_f_pointer( c_loc(res%data(1)), ptr )         ;\
      ptr = val                                           ;\
    end function

  _implementConstructor_(bool,       logical)
  _implementConstructor_(int8,       integer*1)
  _implementConstructor_(int16,      integer*2)
  _implementConstructor_(int32,      integer*4)
  _implementConstructor_(int64,      integer*8)
  _implementConstructor_(real32,     real*4)
  _implementConstructor_(real64,     real*8)
  _implementConstructor_(complex32,  complex*8)
  _implementConstructor_(complex64,  complex*16)
  _implementConstructor_(c_void_ptr, type(c_ptr))
  _implementConstructor_(string,     type(DynamicString_t))
  _implementConstructor_(gref,       type(GenericRef_t))


  function vi_from_charString( val ) result(res)
    character(len=*),   intent(in) :: val
    type(DynamicString_t), pointer :: ptr
    type(VarItem_t),        target :: res

    call vi_reshape( res, static_type(string_var), 0 )
    call c_f_pointer( c_loc(res%data(1)), ptr )
    ptr = val
  end function


  function vi_from_vi( val ) result(res)
    type(VarItem_t), intent(in) :: val
    type(VarItem_t)             :: res

    call vi_reshape( res, val%typeInfo, 0 )
    if (associated( res%typeInfo )) then
      select case (associated( res%typeInfo%assignProc ))
        case (.true.) ; call res%typeInfo%assignProc( res%data, val%data )
        case (.false.); res%data = val%data
      end select
    end if
  end function


# define _implementGetter_(typeId, baseType)                        \
    function _paste(vi_get_,typeId)( self ) result(res)            ;\
      type(VarItem_t), target, intent(in) :: self                  ;\
      baseType,                   pointer :: res                   ;\
      call vi_reshape( self, static_type(_paste(typeId,_var)), 1 ) ;\
      call c_f_pointer( c_loc(self%data(1)), res )                 ;\
    end function

  _implementGetter_(bool,       logical)
  _implementGetter_(int8,       integer*1)
  _implementGetter_(int16,      integer*2)
  _implementGetter_(int32,      integer*4)
  _implementGetter_(int64,      integer*8)
  _implementGetter_(real32,     real*4)
  _implementGetter_(real64,     real*8)
  _implementGetter_(complex32,  complex*8)
  _implementGetter_(complex64,  complex*16)
  _implementGetter_(c_void_ptr, type(c_ptr))
  _implementGetter_(string,     type(DynamicString_t))
  _implementGetter_(gref,       type(GenericRef_t))


# define _implementSetter_(typeId, baseType)          \
    subroutine _paste(vi_assign_,typeId)( lhs, rhs ) ;\
      type(VarItem_t), target, intent(inout) :: lhs  ;\
      baseType,                   intent(in) :: rhs  ;\
      baseType,                      pointer :: ptr  ;\
      call vi_reshape( lhs, static_type(rhs), 1 )    ;\
      call c_f_pointer( c_loc(lhs%data(1)), ptr )    ;\
      ptr = rhs                                      ;\
    end subroutine

  _implementSetter_(bool,       logical)
  _implementSetter_(int8,       integer*1)
  _implementSetter_(int16,      integer*2)
  _implementSetter_(int32,      integer*4)
  _implementSetter_(int64,      integer*8)
  _implementSetter_(real32,     real*4)
  _implementSetter_(real64,     real*8)
  _implementSetter_(complex32,  complex*8)
  _implementSetter_(complex64,  complex*16)
  _implementSetter_(c_void_ptr, type(c_ptr))
  _implementSetter_(string,     type(DynamicString_t))
  _implementSetter_(gref,       type(GenericRef_t))


  subroutine vi_assign_charString( lhs, rhs )
    type(VarItem_t), target, intent(inout) :: lhs
    character(len=*),           intent(in) :: rhs
    type(DynamicString_t),         pointer :: ptr
    call vi_reshape( lhs, static_type(string_var), 1 )
    call c_f_pointer( c_loc(lhs%data(1)), ptr )
    ptr = rhs
  end subroutine


  subroutine vi_assign_vi( lhs, rhs )
    type(VarItem_t), intent(inout) :: lhs
    type(VarItem_t)                :: rhs

    ! can't prevent self assignment ... (since fortran gives us a shallow copy of rhs)
    ! so in case - just do it and let sensitive types handle it themselves.
    call vi_reshape( lhs, rhs%typeInfo, 1 ) !< lhs' get's always hard on assignment!
    if (associated( lhs%typeInfo )) then
      select case (associated( lhs%typeInfo%assignProc ))
        case (.true.) ; call lhs%typeInfo%assignProc( lhs%data, rhs%data )
        case (.false.); lhs%data = rhs%data
      end select
    end if
  end subroutine


# define _implementAssignTo_(typeId, baseType)                \
    subroutine _paste(typeId,_assign_vi)( lhs, rhs )         ;\
      baseType,     intent(inout) :: lhs                     ;\
      type(VarItem_t), intent(in) :: rhs                     ;\
      if (associated( rhs%typeInfo, static_type(lhs) )) then ;\
        lhs = typeId(rhs)                                    ;\
      end if                                                 ;\
    end subroutine

  _implementAssignTo_(bool,       logical)
  _implementAssignTo_(int8,       integer*1)
  _implementAssignTo_(int16,      integer*2)
  _implementAssignTo_(int32,      integer*4)
  _implementAssignTo_(int64,      integer*8)
  _implementAssignTo_(real32,     real*4)
  _implementAssignTo_(real64,     real*8)
  _implementAssignTo_(complex32,  complex*8)
  _implementAssignTo_(complex64,  complex*16)
  _implementAssignTo_(c_void_ptr, type(c_ptr))
  _implementAssignTo_(string,     type(DynamicString_t))
  _implementAssignTo_(gref,       type(GenericRef_t))


# define _implementTypeCheck_(typeId, baseType)                            \
    logical function _paste(vi_is_,typeId)( self ) result(res)            ;\
      type(VarItem_t), intent(in) :: self                                 ;\
      res = associated( self%typeInfo, static_type(_paste(typeId,_var)) ) ;\
    end function

  _implementTypeCheck_(bool,       logical)
  _implementTypeCheck_(int8,       integer*1)
  _implementTypeCheck_(int16,      integer*2)
  _implementTypeCheck_(int32,      integer*4)
  _implementTypeCheck_(int64,      integer*8)
  _implementTypeCheck_(real32,     real*4)
  _implementTypeCheck_(real64,     real*8)
  _implementTypeCheck_(complex32,  complex*8)
  _implementTypeCheck_(complex64,  complex*16)
  _implementTypeCheck_(c_void_ptr, type(c_ptr))
  _implementTypeCheck_(string,     type(DynamicString_t))
  _implementTypeCheck_(gref,       type(GenericRef_t))


# if defined VARITEM_REAL16
  _implementConstructor_(real128, real*16)
  _implementAssignTo_(real128,    real*16)
  _implementGetter_(real128,      real*16)
  _implementSetter_(real128,      real*16)
  _implementTypeCheck_(real128,   real*16)

  _implementConstructor_(complex128, complex*32)
  _implementAssignTo_(complex128,    complex*32)
  _implementGetter_(complex128,      complex*32)
  _implementSetter_(complex128,      complex*32)
  _implementTypeCheck_(complex128,   complex*32)
# endif



  logical function vi_is_valid( self ) result(res)
    type(VarItem_t), intent(in) :: self
    res = associated( self%typeInfo )
  end function


  function vi_dynamic_type( self ) result(res)
    type(VarItem_t), intent(in) :: self
    type(TypeInfo_t),   pointer :: res

    if (associated( self%typeInfo )) then; res => self%typeInfo
                                     else; res => type_void
    end if
  end function


  subroutine vi_delete( self )
    type(VarItem_t) :: self
    
    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%deleteProc )) &
        call self%typeInfo%deleteProc( self%data )
      self%typeInfo => null()
    end if
  end subroutine
 

  subroutine vi_reshape( self, new_typeInfo, hardness )
    type(VarItem_t)                      :: self
    !type(TypeInfo_t), target, intent(in) :: new_typeInfo 
    type(TypeInfo_t), pointer, intent(in) :: new_typeInfo
    integer,                  intent(in) :: hardness

    if (.not. associated( self%typeInfo, new_typeInfo )) then
      ! check if old type needs to be deleted ...
      if (associated( self%typeInfo )) then
        if (associated( self%typeInfo%deleteProc )) &
          call self%typeInfo%deleteProc( self%data )
      end if
      ! check if new type needs to be initialized ...
      if (associated( new_typeInfo%initProc )) &
        call new_typeInfo%initProc( self%data, hardness )
      self%typeInfo => new_typeInfo
    end if
  end subroutine

end module var_item

