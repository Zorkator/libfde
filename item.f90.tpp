
#include "adt/item.fpp"

module adt_item
  use iso_c_binding
  use adt_ref
  use adt_string
  use adt_basetypes
  use adt_list
  implicit none
  private

! declare dummy variables - used to determine each type's storage_size ...
# define _item_type_(typeId, baseType)   baseType :: _paste(typeId,_var);
    _TableOf_item_types_
# undef _item_type_


# define _item_type_(typeId, baseType)   storage_size(_paste(typeId,_var)),

# define _chunkType   integer*8
  _chunkType, parameter :: chunk_var = 0
  integer*4,  parameter :: maxBytes  = max(_TableOf_item_types_ 0)/8
  integer*4,  parameter :: chunkSize = storage_size(chunk_var)/8
  integer*4,  parameter :: numChunks = ceiling( maxBytes / real(chunkSize) )
# undef _item_type_


  type, public :: Item_t
    private
    _chunkType                :: data(numChunks) = 0
    type(TypeInfo_t), pointer :: typeInfo        => null()
  end type


  type, public :: Item_t__impl__
    _chunkType                :: data(numChunks) = 0
    type(TypeInfo_t), pointer :: typeInfo        => null()
  end type


  ! declare Item_of(<type>) interface ...
  ! NOTE: It's named Item_of to avoid ambiguity with Item-interface for type deref.
  interface Item_of
#   define _declareConstructor_(typeId, baseType, bt_import) \
    function _paste(item_of_,typeId)( val ) result(res) ;\
      import Item_t; bt_import                          ;\
      baseType     :: val                               ;\
      type(Item_t) :: res                               ;\
    end function

    _declareConstructor_(bool,        logical,)
    _declareConstructor_(int8,        integer*1,)
    _declareConstructor_(int16,       integer*2,)
    _declareConstructor_(int32,       integer*4,)
    _declareConstructor_(int64,       integer*8,)
    _declareConstructor_(real32,      real*4,)
    _declareConstructor_(real64,      real*8,)
    _declareConstructor_(complex32,   complex*8,)
    _declareConstructor_(complex64,   complex*16,)
    _declareConstructor_(c_void_ptr,  type(c_ptr),    import c_ptr)
    _declareConstructor_(string,      type(String_t), import String_t)
    _declareConstructor_(ref,         type(Ref_t),    import Ref_t)
    _declareConstructor_(charString,  character(len=*),)

    function item_of_refencoding( val ) result(res)
      import Item_t, RefEncoding_t, Ref_t
      type(RefEncoding_t), dimension(:) :: val
      type(Ref_t),              pointer :: ptr
      type(Item_t),              target :: res
    end function
  end interface

!
!# define _item_type_(typeId, baseType) \
!    , _paste(vi_from_,typeId)
!
!  interface Item_of
!    module procedure vi_from_vi, vi_from_charString, vi_from_refencoding &
!                     _TableOf_item_types_
!  end interface
!# undef _item_type_


  ! declare public <typeId>-interfaces for getting type pointers
# define _item_type_(typeId, baseType) \
    interface typeId; module procedure _paste(vi_get_,typeId); end interface; \
    public :: typeId;
    _TableOf_item_types_
# undef _item_type_


  ! declare assignment interface
# define _item_type_(typeId, baseType) \
    , _paste(typeId,_assign_vi), _paste(vi_assign_,typeId)

  interface assignment(=)
    module procedure vi_assign_vi, vi_assign_charString, vi_assign_refencoding &
                     _TableOf_item_types_
  end interface
# undef _item_type_

  ! declare interfaces public

    interface is_valid     ; module procedure vi_is_valid     ; end interface
    interface dynamic_type ; module procedure vi_dynamic_type ; end interface
    interface delete       ; module procedure vi_delete       ; end interface
    interface assign       ; module procedure vi_assign_vi    ; end interface

    public :: Item_of
    public :: is_valid
    public :: dynamic_type
    public :: delete
    public :: assign, assignment(=)


  ! declare public typecheck interfaces ...
# define _item_type_(typeId, baseType) \
    interface _paste(is_,typeId); module procedure _paste(vi_is_,typeId); end interface; \
    public :: _paste(is_,typeId);

    _TableOf_item_types_
# undef _item_type_

  !_TypeGen_declare_RefType( public, Item, type(Item_t), scalar, \
  !     initProc   = vi_initialize, \
  !     assignProc = vi_assign_vi,  \
  !     deleteProc = vi_delete,     \
  !     cloneMode  = _type )

  !_TypeGen_declare_ListNode( public, Item, type(Item_t), scalar )
  
!-----------------
  contains
!-----------------

  !_TypeGen_implementAll()
  
  
  subroutine vi_initialize( self, has_proto, proto )
    type(Item_t) :: self
    integer      :: has_proto
    type(Item_t) :: proto
    self%data     = 0
    self%typeInfo => null()
  end subroutine


!! implement constructor routines
!
!# define _implementConstructor_(typeId, baseType, proto) \
!    function _paste(vi_from_,typeId)( val ) result(res) ;\
!      baseType             :: val                       ;\
!      baseType,    pointer :: ptr                       ;\
!      type(Item_t), target :: res                       ;\
!      if (vi_reshape( res, static_type(val) )) then     ;\
!        call res%typeInfo%initProc( res, 1, proto )     ;\
!      end if                                            ;\
!      call c_f_pointer( c_loc(res%data(1)), ptr )       ;\
!      ptr = val                                         ;\
!    end function
!
!  _implementConstructor_(bool,       logical,   0)
!  _implementConstructor_(int8,       integer*1, 0)
!  _implementConstructor_(int16,      integer*2, 0)
!  _implementConstructor_(int32,      integer*4, 0)
!  _implementConstructor_(int64,      integer*8, 0)
!  _implementConstructor_(real32,     real*4,    0)
!  _implementConstructor_(real64,     real*8,    0)
!  _implementConstructor_(complex32,  complex*8, 0)
!  _implementConstructor_(complex64,  complex*16, 0)
!  _implementConstructor_(c_void_ptr, type(c_ptr), 0)
!  _implementConstructor_(string,     type(String_t), temporary_string)
!  _implementConstructor_(ref,        type(Ref_t),    temporary_ref)
!
!
!  function vi_from_charString( val ) result(res)
!    character(len=*)        :: val
!    type(String_t), pointer :: ptr
!    type(Item_t),    target :: res
!
!    if (vi_reshape( res, static_type(string_var) )) &
!      call res%typeInfo%initProc( res%data, 1, temporary_string )
!    call c_f_pointer( c_loc(res%data(1)), ptr )
!    call assign( ptr, val )
!  end function
!
!
!  function vi_from_refencoding( val ) result(res)
!    type(RefEncoding_t), dimension(:) :: val
!    type(Ref_t),              pointer :: ptr
!    type(Item_t),              target :: res
!    if (vi_reshape( res, static_type(ref_var) )) &
!      call res%typeInfo%initProc( res%data, 1, temporary_ref )
!    call c_f_pointer( c_loc(res%data(1)), ptr )
!    call assign( ptr, val )
!  end function


  function vi_from_vi( val ) result(res)
    type(Item_t) :: val
    type(Item_t) :: res

    if (vi_reshape( res, val%typeInfo )) then
      res%data = 0 !< setting 0 here implies a temporary instance!
      call res%typeInfo%initProc( res%data, 1, res%data )
    end if

    if (associated( res%typeInfo%assignProc )) then
      call res%typeInfo%assignProc( res%data, val%data )
    else
      res%data = val%data
    end if
  end function


# define _implementGetter_(typeId, baseType)                          \
    function _paste(vi_get_,typeId)( self ) result(res)              ;\
      type(Item_t), target :: self                                   ;\
      baseType,    pointer :: res                                    ;\
      if (vi_reshape( self, static_type(_paste(typeId,_var)) )) then ;\
        call self%typeInfo%initProc( self, 0 )                       ;\
      end if                                                         ;\
      call c_f_pointer( c_loc(self%data(1)), res )                   ;\
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
  _implementGetter_(string,     type(String_t))
  _implementGetter_(ref,        type(Ref_t))


# define _implementSetter_(typeId, baseType)          \
    subroutine _paste(vi_assign_,typeId)( lhs, rhs ) ;\
      type(Item_t), target, intent(inout) :: lhs     ;\
      baseType,                intent(in) :: rhs     ;\
      baseType,                   pointer :: ptr     ;\
      if (vi_reshape( lhs, static_type(rhs) )) then  ;\
        call lhs%typeInfo%initProc( lhs, 0 )         ;\
      end if                                         ;\
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
  _implementSetter_(string,     type(String_t))
  _implementSetter_(ref,        type(Ref_t))


  subroutine vi_assign_charString( lhs, rhs )
    type(Item_t), target, intent(inout) :: lhs
    character(len=*),        intent(in) :: rhs
    type(String_t),             pointer :: ptr
    if (vi_reshape( lhs, static_type(string_var) )) &
      call lhs%typeInfo%initProc( lhs, 0 )
    call c_f_pointer( c_loc(lhs%data(1)), ptr )
    call assign( ptr, rhs )
  end subroutine

  
  subroutine vi_assign_refencoding( lhs, rhs )
    type(Item_t),           target, intent(inout) :: lhs
    type(RefEncoding_t), dimension(:), intent(in) :: rhs
    type(Ref_t),                          pointer :: ptr
    if (vi_reshape( lhs, static_type(ref_var) )) &
      call lhs%typeInfo%initProc( lhs, 0 )
    call c_f_pointer( c_loc(lhs%data(1)), ptr )
    call assign( ptr, rhs )
  end subroutine


  subroutine vi_assign_vi( lhs, rhs )
    type(Item_t), intent(inout) :: lhs
    type(Item_t)                :: rhs

    ! can't prevent self assignment ... (since fortran gives us a shallow copy of rhs)
    ! so in case - just do it and let sensitive types handle it themselves.
    if (vi_reshape( lhs, rhs%typeInfo )) &
      call lhs%typeInfo%initProc( lhs, 0 )

    if (associated( lhs%typeInfo%assignProc )) then
      call lhs%typeInfo%assignProc( lhs%data, rhs%data )
    else
      lhs%data = rhs%data
    end if
  end subroutine


# define _implementAssignTo_(typeId, baseType)                \
    subroutine _paste(typeId,_assign_vi)( lhs, rhs )         ;\
      baseType, intent(inout) :: lhs                         ;\
      type(Item_t),    target :: rhs                         ;\
      baseType,       pointer :: ptr                         ;\
      if (associated( static_type(lhs), rhs%typeInfo )) then ;\
        call c_f_pointer( c_loc(rhs%data(1)), ptr )          ;\
        lhs = ptr                                            ;\
      end if                                                 ;\
    end subroutine
  ! CAUTION: Fortran assignment is a real mess - it just copies everything that dares to stand on the
  !   right side of the assignment. Who came up with that crap?
  ! For the pointer check (associated) above it is really important to give static_type() as the
  !   first argument, otherwise Fortrans starts to deref self%typeInfo (WTH!?)
  !   and compares the pointer returned by static_type() to this copy - which is of course different.

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
  _implementAssignTo_(string,     type(String_t))
  _implementAssignTo_(ref,        type(Ref_t))


# define _implementTypeCheck_(typeId, baseType)                            \
    logical function _paste(vi_is_,typeId)( self ) result(res)            ;\
      type(Item_t), intent(in) :: self                                    ;\
      res = associated( static_type(_paste(typeId,_var)), self%typeInfo ) ;\
    end function
  ! For the pointer check (associated) above it is really important to give static_type() as the
  !   first argument, otherwise Fortrans starts to deref self%typeInfo (WTH!?)
  !   and compares the pointer returned by static_type() to this copy - which is of course different.

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
  _implementTypeCheck_(string,     type(String_t))
  _implementTypeCheck_(ref,        type(Ref_t))


# if defined ITEM_REAL16
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
    type(Item_t), intent(in) :: self
    res = associated( self%typeInfo )
  end function


  function vi_dynamic_type( self ) result(res)
    type(Item_t),  intent(in) :: self
    type(TypeInfo_t), pointer :: res

    if (associated( self%typeInfo )) then; res => self%typeInfo
                                     else; res => type_void
    end if
  end function


  recursive &
  subroutine vi_delete( self )
    type(Item_t) :: self
    
    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%deleteProc )) &
        call self%typeInfo%deleteProc( self%data )
      self%typeInfo => null()
    end if
  end subroutine
 

  function vi_reshape( self, new_typeInfo ) result(res)
    type(Item_t)                         :: self
    type(TypeInfo_t), target, intent(in) :: new_typeInfo
    logical                              :: res

    res = .false.
    if (.not. associated( self%typeInfo, new_typeInfo )) then
      ! check if old type needs to be deleted ...
      if (associated( self%typeInfo )) then
        if (associated( self%typeInfo%deleteProc )) &
          call self%typeInfo%deleteProc( self%data )
      end if
      ! return if new type needs to be initialized ...
      res = associated( new_typeInfo%initProc )
      self%typeInfo => new_typeInfo
    end if
  end function

end module

