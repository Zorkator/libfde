
#include "adt/itfUtil.fpp"

module adt_item__
  use adt_item
  use adt_typeinfo
  use adt_string
  use adt_ref
  use iso_c_binding

# define Item_t   Item_t__impl__

  type (String_t) :: string_var
  type (Ref_t)    :: ref_var

  interface
    function item_reshape( self, new_typeInfo ) result(res)
      import Item_t, TypeInfo_t
      type(Item_t)                         :: self
      type(TypeInfo_t), target, intent(in) :: new_typeInfo
      logical                              :: res
    end function
  end interface

end module


!_PROC_EXPORT(item_object_size)
  integer(kind=4) &
  function item_object_size() result(res)
    use adt_item__; implicit none
    type (Item_t) :: tmp
    res = storage_size(tmp) / 8
  end function


!_PROC_EXPORT(item_init_by_proto)
  subroutine item_init_by_proto( self, has_proto, proto )
    use adt_item__
    implicit none
    type(Item_t) :: self
    integer      :: has_proto
    type(Item_t) :: proto
    self%data     = 0
    self%typeInfo => null()
  end subroutine


  function item_reshape( self, new_typeInfo ) result(res)
    use adt_item__, only: Item_t, TypeInfo_t
    implicit none
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


!_PROC_EXPORT(item_is_valid)
  logical function item_is_valid( self ) result(res)
    use adt_item__
    implicit none
    type(Item_t), intent(in) :: self
    res = associated( self%typeInfo )
  end function


!_PROC_EXPORT(item_dynamic_type)
  function item_dynamic_type( self ) result(res)
    use adt_item__
    implicit none
    type(Item_t),  intent(in) :: self
    type(TypeInfo_t), pointer :: res

    if (associated( self%typeInfo )) then; res => self%typeInfo
                                     else; res => type_void
    end if
  end function


!_PROC_EXPORT(item_delete)
  recursive &
  subroutine item_delete( self )
    use adt_item__
    implicit none
    type(Item_t) :: self
    
    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%deleteProc )) &
        call self%typeInfo%deleteProc( self%data )
      self%typeInfo => null()
    end if
  end subroutine


! implement constructor routines

# define _EXPORT_CONSTRUCTOR(typeId)    _PROC_EXPORT(_paste(item_of_,typeId))
# define _implement_constructor_(typeId, baseType, proto) \
  function _paste(item_of_,typeId)( val ) result(res)    ;\
    use adt_item__; implicit none                        ;\
    baseType             :: val                          ;\
    baseType,    pointer :: ptr                          ;\
    type(Item_t), target :: res                          ;\
    if (item_reshape( res, static_type(val) )) then      ;\
      call res%typeInfo%initProc( res, 1, proto )        ;\
    end if                                               ;\
    call c_f_pointer( c_loc(res%data(1)), ptr )          ;\
    ptr = val                                            ;\
  end function

!_EXPORT_CONSTRUCTOR(bool)
  _implement_constructor_(bool,       logical,        0)
!_EXPORT_CONSTRUCTOR(int8)
  _implement_constructor_(int8,       integer*1,      0)
!_EXPORT_CONSTRUCTOR(int16)
  _implement_constructor_(int16,      integer*2,      0)
!_EXPORT_CONSTRUCTOR(int32)
  _implement_constructor_(int32,      integer*4,      0)
!_EXPORT_CONSTRUCTOR(int64)
  _implement_constructor_(int64,      integer*8,      0)
!_EXPORT_CONSTRUCTOR(real32)
  _implement_constructor_(real32,     real*4,         0)
!_EXPORT_CONSTRUCTOR(real64)
  _implement_constructor_(real64,     real*8,         0)
!_EXPORT_CONSTRUCTOR(complex32)
  _implement_constructor_(complex32,  complex*8,      0)
!_EXPORT_CONSTRUCTOR(complex64)
  _implement_constructor_(complex64,  complex*16,     0)
!_EXPORT_CONSTRUCTOR(c_void_ptr)
  _implement_constructor_(c_void_ptr, type(c_ptr),    0)
!_EXPORT_CONSTRUCTOR(string)
  _implement_constructor_(string,     type(String_t), temporary_string)
!_EXPORT_CONSTRUCTOR(ref)
  _implement_constructor_(ref,        type(Ref_t),    temporary_ref)


!_EXPORT_CONSTRUCTOR(charString)
  function item_of_charString( val ) result(res)
    use adt_item__
    implicit none
    character(len=*)        :: val
    type(String_t), pointer :: ptr
    type(Item_t),    target :: res

    if (item_reshape( res, static_type(string_var) )) &
      call res%typeInfo%initProc( res%data, 1, temporary_string )
    call c_f_pointer( c_loc(res%data(1)), ptr )
    call assign( ptr, val )
  end function


!_EXPORT_CONSTRUCTOR(refencoding)
  function item_of_refencoding( val ) result(res)
    use adt_item__
    implicit none
    type(RefEncoding_t), dimension(:) :: val
    type(Ref_t),              pointer :: ptr
    type(Item_t),              target :: res
    if (item_reshape( res, static_type(ref_var) )) &
      call res%typeInfo%initProc( res%data, 1, temporary_ref )
    call c_f_pointer( c_loc(res%data(1)), ptr )
    call assign( ptr, val )
  end function


!_EXPORT_CONSTRUCTOR(item)
  function item_of_item( val ) result(res)
    use adt_item__
    implicit none
    type(Item_t) :: val
    type(Item_t) :: res

    if (item_reshape( res, val%typeInfo )) then
      res%data = 0 !< setting 0 here implies a temporary instance!
      call res%typeInfo%initProc( res%data, 1, res%data )
    end if

    if (associated( res%typeInfo%assignProc )) then
      call res%typeInfo%assignProc( res%data, val%data )
    else
      res%data = val%data
    end if
  end function


! implement getter routines

# define _EXPORT_GETTER(typeId)    _PROC_EXPORT(_paste(item_get_,typeId))
# define _implement_getter_(typeId, baseType)            \
  function _paste(item_get_,typeId)( self ) result(res) ;\
    use adt_item__; implicit none                       ;\
    type(Item_t), target :: self                        ;\
    baseType,    pointer :: res                         ;\
    baseType             :: var                         ;\
    if (item_reshape( self, static_type(var) )) then    ;\
      call self%typeInfo%initProc( self, 0 )            ;\
    end if                                              ;\
    call c_f_pointer( c_loc(self%data(1)), res )        ;\
  end function

!_EXPORT_GETTER(bool)
  _implement_getter_(bool,       logical)
!_EXPORT_GETTER(int8)
  _implement_getter_(int8,       integer*1)
!_EXPORT_GETTER(int16)
  _implement_getter_(int16,      integer*2)
!_EXPORT_GETTER(int32)
  _implement_getter_(int32,      integer*4)
!_EXPORT_GETTER(int64)
  _implement_getter_(int64,      integer*8)
!_EXPORT_GETTER(real32)
  _implement_getter_(real32,     real*4)
!_EXPORT_GETTER(real64)
  _implement_getter_(real64,     real*8)
!_EXPORT_GETTER(complex32)
  _implement_getter_(complex32,  complex*8)
!_EXPORT_GETTER(complex64)
  _implement_getter_(complex64,  complex*16)
!_EXPORT_GETTER(c_void_ptr)
  _implement_getter_(c_void_ptr, type(c_ptr))
!_EXPORT_GETTER(string)
  _implement_getter_(string,     type(String_t))
!_EXPORT_GETTER(ref)
  _implement_getter_(ref,        type(Ref_t))


! implement assignment routines

# define _EXPORT_ASSIGN(typeId)    _PROC_EXPORT(_paste(item_assign_,typeId))
# define _implement_assign_(typeId, baseType)         \
  subroutine _paste(item_assign_,typeId)( lhs, rhs ) ;\
    use adt_item__; implicit none                    ;\
    type(Item_t), target, intent(inout) :: lhs       ;\
    baseType,                intent(in) :: rhs       ;\
    baseType,                   pointer :: ptr       ;\
    if (item_reshape( lhs, static_type(rhs) )) then  ;\
      call lhs%typeInfo%initProc( lhs, 0 )           ;\
    end if                                           ;\
    call c_f_pointer( c_loc(lhs%data(1)), ptr )      ;\
    ptr = rhs                                        ;\
  end subroutine

!_EXPORT_ASSIGN(bool)
  _implement_assign_(bool,       logical)
!_EXPORT_ASSIGN(int8)
  _implement_assign_(int8,       integer*1)
!_EXPORT_ASSIGN(int16)
  _implement_assign_(int16,      integer*2)
!_EXPORT_ASSIGN(int32)
  _implement_assign_(int32,      integer*4)
!_EXPORT_ASSIGN(int64)
  _implement_assign_(int64,      integer*8)
!_EXPORT_ASSIGN(real32)
  _implement_assign_(real32,     real*4)
!_EXPORT_ASSIGN(real64)
  _implement_assign_(real64,     real*8)
!_EXPORT_ASSIGN(complex32)
  _implement_assign_(complex32,  complex*8)
!_EXPORT_ASSIGN(complex64)
  _implement_assign_(complex64,  complex*16)
!_EXPORT_ASSIGN(c_void_ptr)
  _implement_assign_(c_void_ptr, type(c_ptr))
!_EXPORT_ASSIGN(string)
  _implement_assign_(string,     type(String_t))
!_EXPORT_ASSIGN(ref)
  _implement_assign_(ref,        type(Ref_t))


!_EXPORT_ASSIGN(charString)
  subroutine item_assign_charString( lhs, rhs )
    use adt_item__
    implicit none
    type(Item_t), target, intent(inout) :: lhs
    character(len=*),        intent(in) :: rhs
    type(String_t),             pointer :: ptr
    if (item_reshape( lhs, static_type(string_var) )) &
      call lhs%typeInfo%initProc( lhs, 0 )
    call c_f_pointer( c_loc(lhs%data(1)), ptr )
    call assign( ptr, rhs )
  end subroutine

  
!_EXPORT_ASSIGN(refencoding)
  subroutine item_assign_refencoding( lhs, rhs )
    use adt_item__
    implicit none
    type(Item_t),           target, intent(inout) :: lhs
    type(RefEncoding_t), dimension(:), intent(in) :: rhs
    type(Ref_t),                          pointer :: ptr
    if (item_reshape( lhs, static_type(ref_var) )) &
      call lhs%typeInfo%initProc( lhs, 0 )
    call c_f_pointer( c_loc(lhs%data(1)), ptr )
    call assign( ptr, rhs )
  end subroutine


!_EXPORT_ASSIGN(item)
  subroutine item_assign_item( lhs, rhs )
    use adt_item__
    implicit none
    type(Item_t), intent(inout) :: lhs
    type(Item_t)                :: rhs

    ! can't prevent self assignment ... (since fortran gives us a shallow copy of rhs)
    ! so in case - just do it and let sensitive types handle it themselves.
    if (item_reshape( lhs, rhs%typeInfo )) &
      call lhs%typeInfo%initProc( lhs, 0 )

    if (associated( lhs%typeInfo%assignProc )) then
      call lhs%typeInfo%assignProc( lhs%data, rhs%data )
    else
      lhs%data = rhs%data
    end if
  end subroutine


! implement assign-to routines

# define _EXPORT_ASSIGN_TO(typeId)    _PROC_EXPORT(_paste(typeId,_assign_item))
# define _implement_assign_to_(typeId, baseType)            \
  subroutine _paste(typeId,_assign_item)( lhs, rhs )       ;\
    use adt_item__; implicit none                          ;\
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

!_EXPORT_ASSIGN_TO(bool)
  _implement_assign_to_(bool,       logical)
!_EXPORT_ASSIGN_TO(int8)
  _implement_assign_to_(int8,       integer*1)
!_EXPORT_ASSIGN_TO(int16)
  _implement_assign_to_(int16,      integer*2)
!_EXPORT_ASSIGN_TO(int32)
  _implement_assign_to_(int32,      integer*4)
!_EXPORT_ASSIGN_TO(int64)
  _implement_assign_to_(int64,      integer*8)
!_EXPORT_ASSIGN_TO(real32)
  _implement_assign_to_(real32,     real*4)
!_EXPORT_ASSIGN_TO(real64)
  _implement_assign_to_(real64,     real*8)
!_EXPORT_ASSIGN_TO(complex32)
  _implement_assign_to_(complex32,  complex*8)
!_EXPORT_ASSIGN_TO(complex64)
  _implement_assign_to_(complex64,  complex*16)
!_EXPORT_ASSIGN_TO(c_void_ptr)
  _implement_assign_to_(c_void_ptr, type(c_ptr))
!_EXPORT_ASSIGN_TO(string)
  _implement_assign_to_(string,     type(String_t))
!_EXPORT_ASSIGN_TO(ref)
  _implement_assign_to_(ref,        type(Ref_t))


! implement type check routines

# define _EXPORT_TYPECHECK(typeId)    _PROC_EXPORT(_paste(item_is_,typeId))
# define _implement_typecheck_(typeId, baseType)                \
  logical function _paste(item_is_,typeId)( self ) result(res) ;\
    use adt_item__; implicit none                              ;\
    type(Item_t), intent(in) :: self                           ;\
    baseType                 :: var                            ;\
    res = associated( static_type(var), self%typeInfo )        ;\
  end function
  ! For the pointer check (associated) above it is really important to give static_type() as the
  !   first argument, otherwise Fortrans starts to deref self%typeInfo (WTH!?)
  !   and compares the pointer returned by static_type() to this copy - which is of course different.

!_EXPORT_TYPECHECK(bool)
  _implement_typecheck_(bool,       logical)
!_EXPORT_TYPECHECK(int8)
  _implement_typecheck_(int8,       integer*1)
!_EXPORT_TYPECHECK(int16)
  _implement_typecheck_(int16,      integer*2)
!_EXPORT_TYPECHECK(int32)
  _implement_typecheck_(int32,      integer*4)
!_EXPORT_TYPECHECK(int64)
  _implement_typecheck_(int64,      integer*8)
!_EXPORT_TYPECHECK(real32)
  _implement_typecheck_(real32,     real*4)
!_EXPORT_TYPECHECK(real64)
  _implement_typecheck_(real64,     real*8)
!_EXPORT_TYPECHECK(complex32)
  _implement_typecheck_(complex32,  complex*8)
!_EXPORT_TYPECHECK(complex64)
  _implement_typecheck_(complex64,  complex*16)
!_EXPORT_TYPECHECK(c_void_ptr)
  _implement_typecheck_(c_void_ptr, type(c_ptr))
!_EXPORT_TYPECHECK(string)
  _implement_typecheck_(string,     type(String_t))
!_EXPORT_TYPECHECK(ref)
  _implement_typecheck_(ref,        type(Ref_t))


# if defined ITEM_REAL16
!_EXPORT_CONSTRUCTOR(complex128)
  _implement_constructor_(real128, real*16)
!_EXPORT_GETTER(complex128)
  _implement_getter_(real128,      real*16)
!_EXPORT_ASSIGN(complex128)
  _implement_assign_(real128,      real*16)
!_EXPORT_ASSIGN_TO(complex128)
  _implement_assign_to_(real128,   real*16)
!_EXPORT_TYPECHECK(real128)
  _implement_typecheck_(real128,   real*16)

!_EXPORT_CONSTRUCTOR(complex128)
  _implement_constructor_(complex128, complex*32)
!_EXPORT_GETTER(complex128)
  _implement_getter_(complex128,      complex*32)
!_EXPORT_ASSIGN(complex128)
  _implement_assign_(complex128,      complex*32)
!_EXPORT_ASSIGN_TO(complex128)
  _implement_assign_to_(complex128,   complex*32)
!_EXPORT_TYPECHECK(complex128)
  _implement_typecheck_(complex128,   complex*32)
# endif


