
#include "adt/itfUtil.fpp"

module impl_item__
  use adt_item
  use adt_typeinfo
  use adt_string
  use adt_ref
  use adt_basetypes
  use adt_memoryref
  use iso_c_binding

# define Item_i   Item_t__impl__

  type Item_wrap_t
    type(Item_i), pointer :: ptr
  end type

  type (String_t)                    :: string_var
  type (Ref_t)                       :: ref_var
  procedure(UserAssignment), pointer :: user_assignment_ => null()

  interface
    logical &
    function item_reshape_( self, new_typeInfo )
      import Item_i, TypeInfo_t
      type(Item_i)                         :: self
      type(TypeInfo_t), target, intent(in) :: new_typeInfo
    end function

    logical &
    function auto_assignable_( src, lhsPtr, lhsType, rhs )
      import c_ptr, TypeInfo_t, Item_i
      type(c_ptr),  intent(out) :: src
      type(c_ptr)               :: lhsPtr
      type(TypeInfo_t), pointer :: lhsType, rhsType
      type(Item_i),      target :: rhs
    end function

    function item_content_type( self ) result(res)
      import Item_i, TypeInfo_t
      type(Item_i),  intent(in) :: self
      type(TypeInfo_t), pointer :: res
    end function

    function item_get_ref( self ) result(res)
      import Item_i, Ref_t
      type(Item_i), target :: self
      type(Ref_t), pointer :: res
    end function

    function item_get_data_cptr( self ) result(res)
      import
      type(Item_t) :: self
      type(c_ptr)  :: res
    end function

    integer &
    function item_resolve_data( ctgt, ti, ref, item )
      import
      type(c_ptr),         intent(out) :: ctgt
      type(TypeInfo_t)                 :: ti
      type(Ref_t),    optional, target :: ref
      type(Item_t),   optional, target :: item
    end function
  end interface

end module


!_PROC_EXPORT(item_object_size_c)
  integer(kind=4) &
  function item_object_size_c() result(res)
    use impl_item__; implicit none
    type (Item_i) :: tmp
    res = storage_size(tmp) / 8
  end function


!_PROC_EXPORT(item_init_by_item_c)
  subroutine item_init_by_item_c( self, has_proto, proto )
    use impl_item__
    implicit none
    type(Item_i),    intent(inout) :: self
    integer(kind=4), intent(in)    :: has_proto
    type(Item_i),    intent(in)    :: proto
    self%data     = 0
    self%typeInfo => null()
    if (has_proto /= 0) &
      call item_assign_item_c( self, proto )
  end subroutine


  function item_reshape_( self, new_typeInfo ) result(res)
    use impl_item__, only: Item_i, TypeInfo_t
    implicit none
    type(Item_i)                         :: self
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


!_PROC_EXPORT(item_on_type_mismatch_c)
  subroutine item_on_type_mismatch_c( handler )
    use impl_item__; implicit none
    procedure(UserAssignment) :: handler
    user_assignment_ => handler
  end subroutine


!_PROC_EXPORT(item_is_valid_c)
  logical function item_is_valid_c( self ) result(res)
    use impl_item__
    implicit none
    type(Item_i), intent(in) :: self
    res = associated( self%typeInfo )
  end function


!_PROC_EXPORT(item_content_type)
  function item_content_type( self ) result(res)
    ! NOTE: in contrast to the interface argument self is declared optional here!
    !       This is because we need to handle null pointers here, while we do not allow
    !         calling content_type with self actually missing!
    use impl_item__, only: Item_i, TypeInfo_t, void_type
    implicit none
    type(Item_i), optional, intent(in) :: self
    type(TypeInfo_t),          pointer :: res

    res => null()
    if (present(self))         res => self%typeInfo
    if (.not. associated(res)) res => void_type()
  end function


!_PROC_EXPORT(item_content_type_c)
  subroutine item_content_type_c( res, self )
    ! NOTE: in contrast to the interface argument self is declared optional here!
    !       This is because we need to handle null pointers here, while we do not allow
    !         calling content_type with self actually missing!
    use impl_item__
    implicit none
    type(TypeSpecs_t),      intent(inout) :: res
    type(Item_i), optional, intent(in)    :: self
    type(TypeInfo_t),             pointer :: ptr

    ptr => null()
    if (present(self))         ptr => self%typeInfo
    if (.not. associated(ptr)) ptr => void_type()
    res = ptr%typeSpecs
  end subroutine


!_PROC_EXPORT(item_dynamic_type)
  function item_dynamic_type( self ) result(res)
    ! NOTE: in contrast to the interface argument self is declared optional here!
    !       This is because we need to handle null pointers here, while we do not allow
    !         calling dynamic_type with self actually missing!
    use impl_item__, only: Ref_t, Item_i, TypeInfo_t
    implicit none
    type(Item_i), optional, intent(in) :: self
    type(TypeInfo_t),          pointer :: res

    interface
      function item_resolve_type( ref, item ) result(ti)
        import
        type(Ref_t),  optional, target :: ref
        type(Item_i), optional, target :: item
        type(TypeInfo_t),      pointer :: ti
      end function
    end interface
    res => item_resolve_type( item=self )
  end function


!_PROC_EXPORT(item_delete_c)
  recursive &
  subroutine item_delete_c( self )
    use impl_item__
    implicit none
    type(Item_i) :: self
    
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
    use iso_c_binding                                    ;\
    use impl_item__; implicit none                       ;\
    baseType             :: val                          ;\
    baseType,    pointer :: ptr                          ;\
    type(Item_i), target :: res                          ;\
    if (item_reshape_( res, type_of(val) )) then         ;\
      call res%typeInfo%initProc( res%data, 1, proto )   ;\
    end if                                               ;\
    call c_f_pointer( c_loc(res%data(1)), ptr )          ;\
    ptr = val                                            ;\
  end function

!_EXPORT_CONSTRUCTOR(bool1)
  _implement_constructor_(bool1,      logical*1,      0)
!_EXPORT_CONSTRUCTOR(bool2)
  _implement_constructor_(bool2,      logical*2,      0)
!_EXPORT_CONSTRUCTOR(bool4)
  _implement_constructor_(bool4,      logical*4,      0)
!_EXPORT_CONSTRUCTOR(bool8)
  _implement_constructor_(bool8,      logical*8,      0)
!_EXPORT_CONSTRUCTOR(int1)
  _implement_constructor_(int1,       integer*1,      0)
!_EXPORT_CONSTRUCTOR(int2)
  _implement_constructor_(int2,       integer*2,      0)
!_EXPORT_CONSTRUCTOR(int4)
  _implement_constructor_(int4,       integer*4,      0)
!_EXPORT_CONSTRUCTOR(int8)
  _implement_constructor_(int8,       integer*8,      0)
!_EXPORT_CONSTRUCTOR(real4)
  _implement_constructor_(real4,      real*4,         0)
!_EXPORT_CONSTRUCTOR(real8)
  _implement_constructor_(real8,      real*8,         0)
!_EXPORT_CONSTRUCTOR(complex8)
  _implement_constructor_(complex8,   complex*8,      0)
!_EXPORT_CONSTRUCTOR(complex16)
  _implement_constructor_(complex16,  complex*16,     0)
!_EXPORT_CONSTRUCTOR(c_void_ptr)
  _implement_constructor_(c_void_ptr, type(c_ptr),    0)
!_EXPORT_CONSTRUCTOR(string)
  _implement_constructor_(string,     type(String_t), temporary_string)
!_EXPORT_CONSTRUCTOR(ref)
  _implement_constructor_(ref,        type(Ref_t),    temporary_ref)


!_EXPORT_CONSTRUCTOR(charstring)
  function item_of_charstring( val ) result(res)
    use impl_item__
    implicit none
    character(len=*)        :: val
    type(String_t), pointer :: ptr
    type(Item_i),    target :: res

    if (item_reshape_( res, type_of(string_var) )) &
      call res%typeInfo%initProc( res%data, 1, temporary_string )
    call c_f_pointer( c_loc(res%data(1)), ptr )
    call assign( ptr, val )
  end function


!_EXPORT_CONSTRUCTOR(refencoding)
  function item_of_refencoding( val ) result(res)
    use impl_item__
    implicit none
    type(RefEncoding_t), dimension(:) :: val
    type(Ref_t),              pointer :: ptr
    type(Item_i),              target :: res
    if (item_reshape_( res, type_of(ref_var) )) &
      call res%typeInfo%initProc( res%data, 1, temporary_ref )
    call c_f_pointer( c_loc(res%data(1)), ptr )
    call assign( ptr, val )
  end function


!_EXPORT_CONSTRUCTOR(item)
  function item_of_item( val ) result(res)
    use impl_item__
    implicit none
    type(Item_i) :: val
    type(Item_i) :: res
    call item_assign_item_c( res, val )
  end function


! implement getter routines

# define _EXPORT_GETTER(typeId)    _PROC_EXPORT(_paste(item_get_,typeId))
# define _implement_getter_(typeId, baseType)              \
  function _paste(item_get_,typeId)( self ) result(res)   ;\
    use impl_item__, only: Item_i, Ref_t, String_t, c_ptr ,\
            type_of, item_reshape_, c_f_pointer, c_loc    ;\
    type(Item_i), target :: self                          ;\
    baseType,    pointer :: res                           ;\
    baseType             :: var                           ;\
    if (item_reshape_( self, type_of(var) )) then         ;\
      call self%typeInfo%initProc( self%data, 0 )         ;\
    end if                                                ;\
    call c_f_pointer( c_loc(self%data(1)), res )          ;\
  end function

!_EXPORT_GETTER(bool1)
  _implement_getter_(bool1,      logical*1)
!_EXPORT_GETTER(bool2)
  _implement_getter_(bool2,      logical*2)
!_EXPORT_GETTER(bool4)
  _implement_getter_(bool4,      logical*4)
!_EXPORT_GETTER(bool8)
  _implement_getter_(bool8,      logical*8)
!_EXPORT_GETTER(int1)
  _implement_getter_(int1,       integer*1)
!_EXPORT_GETTER(int2)
  _implement_getter_(int2,       integer*2)
!_EXPORT_GETTER(int4)
  _implement_getter_(int4,       integer*4)
!_EXPORT_GETTER(int8)
  _implement_getter_(int8,       integer*8)
!_EXPORT_GETTER(real4)
  _implement_getter_(real4,      real*4)
!_EXPORT_GETTER(real8)
  _implement_getter_(real8,      real*8)
!_EXPORT_GETTER(complex8)
  _implement_getter_(complex8,   complex*8)
!_EXPORT_GETTER(complex16)
  _implement_getter_(complex16,  complex*16)
!_EXPORT_GETTER(c_void_ptr)
  _implement_getter_(c_void_ptr, type(c_ptr))
!_EXPORT_GETTER(string)
  _implement_getter_(string,     type(String_t))
!_EXPORT_GETTER(ref)
  _implement_getter_(ref,        type(Ref_t))


!_PROC_EXPORT(item_memoryref_c)
  subroutine item_memoryref_c( res, self )
    use impl_item__
    implicit none
    type(Item_i), target, intent(in) :: self
    type(MemoryRef_t), intent(inout) :: res
    
    if (associated( self%typeInfo )) then
      res%loc = c_loc(self%data(1))
      res%len = self%typeInfo%typeSpecs%byteSize
    else
      res = null_ref
    end if
  end subroutine


! implement assignment routines

# define _EXPORT_ASSIGN(typeId)    _PROC_EXPORT(_paste(item_assign_,typeId)_c)
# define _implement_assign_(typeId, baseType)           \
  subroutine _paste(item_assign_,typeId)_c( lhs, rhs ) ;\
    use impl_item__; implicit none                     ;\
    type(Item_i), target, intent(inout) :: lhs         ;\
    baseType,                intent(in) :: rhs         ;\
    baseType,                   pointer :: ptr         ;\
    if (item_reshape_( lhs, type_of(rhs) )) then       ;\
      call lhs%typeInfo%initProc( lhs%data, 0 )        ;\
    end if                                             ;\
    call c_f_pointer( c_loc(lhs%data(1)), ptr )        ;\
    ptr = rhs                                          ;\
  end subroutine

!_EXPORT_ASSIGN(bool1)
  _implement_assign_(bool1,      logical*1)
!_EXPORT_ASSIGN(bool2)
  _implement_assign_(bool2,      logical*2)
!_EXPORT_ASSIGN(bool4)
  _implement_assign_(bool4,      logical*4)
!_EXPORT_ASSIGN(bool8)
  _implement_assign_(bool8,      logical*8)
!_EXPORT_ASSIGN(int1)
  _implement_assign_(int1,       integer*1)
!_EXPORT_ASSIGN(int2)
  _implement_assign_(int2,       integer*2)
!_EXPORT_ASSIGN(int4)
  _implement_assign_(int4,       integer*4)
!_EXPORT_ASSIGN(int8)
  _implement_assign_(int8,       integer*8)
!_EXPORT_ASSIGN(real4)
  _implement_assign_(real4,      real*4)
!_EXPORT_ASSIGN(real8)
  _implement_assign_(real8,      real*8)
!_EXPORT_ASSIGN(complex8)
  _implement_assign_(complex8,   complex*8)
!_EXPORT_ASSIGN(complex16)
  _implement_assign_(complex16,  complex*16)
!_EXPORT_ASSIGN(c_void_ptr)
  _implement_assign_(c_void_ptr, type(c_ptr))
!_EXPORT_ASSIGN(string)
  _implement_assign_(string,     type(String_t))
!_EXPORT_ASSIGN(ref)
  _implement_assign_(ref,        type(Ref_t))


!_EXPORT_ASSIGN(charstring)
  subroutine item_assign_charstring_c( lhs, rhs )
    use impl_item__
    implicit none
    type(Item_i), target, intent(inout) :: lhs
    character(len=*),        intent(in) :: rhs
    type(String_t),             pointer :: ptr
    if (item_reshape_( lhs, type_of(string_var) )) &
      call lhs%typeInfo%initProc( lhs%data, 0 )
    call c_f_pointer( c_loc(lhs%data(1)), ptr )
    call assign( ptr, rhs )
  end subroutine

  
!_PROC_EXPORT(item_assign_refencoding_)
  subroutine item_assign_refencoding_( lhs, rhs )
    use impl_item__
    implicit none
    type(Item_i),           target, intent(inout) :: lhs
    type(RefEncoding_t), dimension(:), intent(in) :: rhs
    type(Ref_t),                          pointer :: ptr
    if (item_reshape_( lhs, type_of(ref_var) )) &
      call lhs%typeInfo%initProc( lhs%data, 0 )
    call c_f_pointer( c_loc(lhs%data(1)), ptr )
    call assign( ptr, rhs )
  end subroutine


!_EXPORT_ASSIGN(item)
  subroutine item_assign_item_c( lhs, rhs )
    use impl_item__
    implicit none
    type(Item_i), intent(inout) :: lhs
    type(Item_i)                :: rhs

    if (associated( rhs%typeInfo )) then
      ! can't prevent self assignment ... (since fortran gives us a shallow copy of rhs)
      ! so in case - just do it and let sensitive types handle it themselves.
      if (item_reshape_( lhs, rhs%typeInfo )) &
        call lhs%typeInfo%initProc( lhs%data, 0 )

      if (associated( lhs%typeInfo%assignProc )) then
        call lhs%typeInfo%assignProc( lhs%data, rhs%data )
      else
        lhs%data = rhs%data
      end if
    else
      call item_delete_c( lhs )
    end if
  end subroutine


! implement assign-to routines

  logical &
  function auto_assignable_( src, lhsPtr, lhsType, rhs ) result(res)
    use impl_item__, only: c_ptr, TypeInfo_t, Item_i, Ref_t, item_content_type, item_get_ref, user_assignment_ &
                         , temporary_ref, type_of, content_type, ref, cptr, c_loc
    implicit none          
    type(c_ptr),  intent(out) :: src
    type(c_ptr)               :: lhsPtr
    type(TypeInfo_t), pointer :: lhsType, rhsType, refType
    type(Item_i),      target :: rhs
    type(Ref_t),      pointer :: refPtr
    integer,        parameter :: stdout = 6
    integer                   :: stat
  
    rhsType => item_content_type(rhs)
    res     =  associated( lhsType, rhsType )
    if (res) then
      ! types match
      src = c_loc(rhs%data(1))
    else
      ! type mismatch - check if rhs is reference
      refType => type_of(temporary_ref)
      if (associated( rhsType, refType )) then
        ! rhs is reference: deref as far as possible ...
        refPtr => item_get_ref(rhs)
        do while (.true.)
          rhsType => content_type(refPtr)
          if (.not. associated( rhsType, refType )) &
            exit
          refPtr => ref(refPtr)
        end do
        ! get target pointer and re-check type
        src = cptr(refPtr)
        res = associated( lhsType, rhsType )
      end if
    
      ! types don't match ...
      if (.not. res) then
        ! try user-assignment or write error to stdout
        if (associated( user_assignment_ )) then
          call user_assignment_( lhsPtr, lhsType%typeSpecs, src, rhsType%typeSpecs )
        else
          write(stdout,*,iostat=stat) "ERROR: skipping illegal assignment " // &
            "<" // trim(lhsType%typeId) // "> := <" // trim(rhsType%typeId) // ">"
        end if
      end if
    end if
  end function


# define _EXPORT_ASSIGN_TO(typeId)    _PROC_EXPORT(_paste(item_assign_to_,typeId)_c)
# define _implement_assign_to_(typeId, baseType)                      \
  subroutine _paste(item_assign_to_,typeId)_c( lhs, rhs )            ;\
    use impl_item__; implicit none                                   ;\
    baseType,     target, intent(inout) :: lhs                       ;\
    type(Item_i), target                :: rhs                       ;\
    baseType,                   pointer :: ptr                       ;\
    type(c_ptr)                         :: src                       ;\
    if (auto_assignable_( src, c_loc(lhs), type_of(lhs), rhs )) then ;\
      call c_f_pointer( src, ptr )                                   ;\
      lhs = ptr                                                      ;\
    end if                                                           ;\
  end subroutine

!_EXPORT_ASSIGN_TO(bool1)
  _implement_assign_to_(bool1,      logical*1)
!_EXPORT_ASSIGN_TO(bool2)
  _implement_assign_to_(bool2,      logical*2)
!_EXPORT_ASSIGN_TO(bool4)
  _implement_assign_to_(bool4,      logical*4)
!_EXPORT_ASSIGN_TO(bool8)
  _implement_assign_to_(bool8,      logical*8)
!_EXPORT_ASSIGN_TO(int1)
  _implement_assign_to_(int1,       integer*1)
!_EXPORT_ASSIGN_TO(int2)
  _implement_assign_to_(int2,       integer*2)
!_EXPORT_ASSIGN_TO(int4)
  _implement_assign_to_(int4,       integer*4)
!_EXPORT_ASSIGN_TO(int8)
  _implement_assign_to_(int8,       integer*8)
!_EXPORT_ASSIGN_TO(real4)
  _implement_assign_to_(real4,      real*4)
!_EXPORT_ASSIGN_TO(c_void_ptr)
  _implement_assign_to_(c_void_ptr, type(c_ptr))
!_EXPORT_ASSIGN_TO(string)
  _implement_assign_to_(string,     type(String_t))
!_EXPORT_ASSIGN_TO(ref)
  _implement_assign_to_(ref,        type(Ref_t))


! implement type check routines

# define _EXPORT_TYPECHECK(typeId)    _PROC_EXPORT(_paste(item_is_,typeId)_c)
# define _implement_typecheck_(typeId, baseType)                  \
  logical function _paste(item_is_,typeId)_c( self ) result(res) ;\
    use impl_item__; implicit none                               ;\
    type(Item_i), intent(in) :: self                             ;\
    baseType                 :: var                              ;\
    res = associated( type_of(var), self%typeInfo )          ;\
  end function
  ! For the pointer check (associated) above it is really important to give type_of() as the
  !   first argument, otherwise Fortrans starts to deref self%typeInfo (WTH!?)
  !   and compares the pointer returned by type_of() to this copy - which is of course different.

!_EXPORT_TYPECHECK(bool1)
  _implement_typecheck_(bool1,      logical*1)
!_EXPORT_TYPECHECK(bool2)
  _implement_typecheck_(bool2,      logical*2)
!_EXPORT_TYPECHECK(bool4)
  _implement_typecheck_(bool4,      logical*4)
!_EXPORT_TYPECHECK(bool8)
  _implement_typecheck_(bool8,      logical*8)
!_EXPORT_TYPECHECK(int1)
  _implement_typecheck_(int1,       integer*1)
!_EXPORT_TYPECHECK(int2)
  _implement_typecheck_(int2,       integer*2)
!_EXPORT_TYPECHECK(int4)
  _implement_typecheck_(int4,       integer*4)
!_EXPORT_TYPECHECK(int8)
  _implement_typecheck_(int8,       integer*8)
!_EXPORT_TYPECHECK(real4)
  _implement_typecheck_(real4,      real*4)
!_EXPORT_TYPECHECK(real8)
  _implement_typecheck_(real8,      real*8)
!_EXPORT_TYPECHECK(complex8)
  _implement_typecheck_(complex8,   complex*8)
!_EXPORT_TYPECHECK(complex16)
  _implement_typecheck_(complex16,  complex*16)
!_EXPORT_TYPECHECK(c_void_ptr)
  _implement_typecheck_(c_void_ptr, type(c_ptr))
!_EXPORT_TYPECHECK(string)
  _implement_typecheck_(string,     type(String_t))
!_EXPORT_TYPECHECK(ref)
  _implement_typecheck_(ref,        type(Ref_t))


! implement dynamic_cast routines
! NOTE: in contrast to the interface argument self is declared optional here!
!       This is because we need to handle null pointers here, while we do not allow
!         calling dynamic_cast with self actually missing!
# define _EXPORT_DYNAMIC_CAST(typeId) _PROC_EXPORT(_paste(item_dynamic_cast_,typeId)_c)
# define _implement_dynamic_cast_(typeId, baseType) \
  logical function _paste(item_dynamic_cast_,typeId)_c( tgt, self ) result(res) ;\
    use impl_item__; implicit none                                              ;\
    baseType,              pointer :: tgt                                       ;\
    type(Item_t), optional, target :: self                                      ;\
    baseType                       :: var                                       ;\
    type(c_ptr)                    :: cp                                        ;\
    select case (item_resolve_data( cp, type_of(var), item=self ))              ;\
      case (1); call c_f_unwrap( cp, tgt );  res = .true.                       ;\
      case (2); call c_f_pointer( cp, tgt ); res = .true.                       ;\
      case default;           tgt => null(); res = .false.                      ;\
    end select                                                                  ;\
  end function

!_EXPORT_DYNAMIC_CAST(string)
  _implement_dynamic_cast_(string,     type(String_t))
!_EXPORT_DYNAMIC_CAST(ref)
  _implement_dynamic_cast_(ref,        type(Ref_t))


# if defined ITEM_REAL16
!_EXPORT_CONSTRUCTOR(real16)
  _implement_constructor_(real16,     real*16)
!_EXPORT_GETTER(real16)
  _implement_getter_(real16,          real*16)
!_EXPORT_ASSIGN(real16)
  _implement_assign_(real16,          real*16)
!_EXPORT_ASSIGN_TO(real16)
  _implement_assign_to_(real16,       real*16)
!_EXPORT_TYPECHECK(real16)
  _implement_typecheck_(real16,       real*16)

!_EXPORT_CONSTRUCTOR(complex32)
  _implement_constructor_(complex32,  complex*32)
!_EXPORT_GETTER(complex32)
  _implement_getter_(complex32,       complex*32)
!_EXPORT_ASSIGN(complex32)
  _implement_assign_(complex32,       complex*32)
!_EXPORT_ASSIGN_TO(complex32)
  _implement_assign_to_(complex32,    complex*32)
!_EXPORT_TYPECHECK(complex32)
  _implement_typecheck_(complex32,    complex*32)
# endif


  recursive &
  subroutine item_accept_wrap_( wrap, itemType, vstr )
    use impl_item__
    use adt_visitor
    implicit none
    type(Item_wrap_t)         :: wrap
    type(TypeInfo_t)          :: itemType
    type(Visitor_t)           :: vstr
    type(TypeInfo_t), pointer :: ti

    ti => item_content_type(wrap%ptr)
    call c_f_pointer( c_loc(wrap%ptr%data), wrap%ptr )
    call ti%acceptProc( wrap, ti, vstr )
  end subroutine


  recursive &
  subroutine item_stream_wrap_( wrap, itemType, outs )
    use impl_item__
    use adt_ostream
    implicit none
    type(Item_wrap_t)         :: wrap
    type(TypeInfo_t)          :: itemType
    type(ostream_t)           :: outs
    type(TypeInfo_t), pointer :: ti
    type(void_t)              :: dataWrap
    character(len=32)         :: buff
    
    ti => item_content_type(wrap%ptr)
    call c_f_pointer( c_loc(wrap%ptr%data), dataWrap%ptr )
    call ti%streamProc( dataWrap, ti, outs )
  end subroutine

! definitions/calls to macros moved to avoid non-understood crash of ifort!!!
!_EXPORT_ASSIGN_TO(real8)
  _implement_assign_to_(real8,      real*8)
!_EXPORT_ASSIGN_TO(complex8)
  _implement_assign_to_(complex8,   complex*8)
!_EXPORT_ASSIGN_TO(complex16)
  _implement_assign_to_(complex16,  complex*16)


  function item_get_data_cptr( self ) result(res)
    use impl_item__, only: Item_i, c_ptr, c_loc
    type(Item_i), target :: self
    type(c_ptr)          :: res
    res = c_loc(self%data(1))
  end function


!_PROC_EXPORT(item_resolve_data)
  integer &
  function item_resolve_data( ctgt, ti, ref_, item_ ) result(res)
    use impl_item__, only: TypeInfo_t, Ref_t, Item_t, type_of, temporary_ref
    use impl_item__, only: content_type, ref_get_typereference, ref, item, item_get_data_cptr
    use iso_c_binding
    implicit none
    type(c_ptr),         intent(out) :: ctgt
    type(TypeInfo_t),         target :: ti
    type(Ref_t),    optional, target :: ref_
    type(Item_t),   optional, target :: item_
    type(TypeInfo_t),        pointer :: dt, refType, itemType

    refType  => type_of(temporary_ref)
    itemType => type_of(item_)

    if     (present(ref_))  then; call visit_ref( ref_ )
    elseif (present(item_)) then; call visit_item( item_ )
    else
      ctgt = C_NULL_PTR
      res  = 0
    end if

    contains

    recursive &
    subroutine visit_ref( r )
      type(Ref_t) :: r
      
      dt => content_type( r )
      if (associated( dt, ti )) then
        ctgt = ref_get_typereference( r )
        res  = 1
      else if (associated( dt, refType ))  then; call visit_ref( ref(r) )
      else if (associated( dt, itemType )) then; call visit_item( item(r) )
      else
        ctgt = C_NULL_PTR
        res  = 0
      end if
    end subroutine

    recursive &
    subroutine visit_item( i )
      type(Item_t) :: i

      dt => content_type( i )
      if (associated( dt, ti )) then
        ctgt = item_get_data_cptr( i )
        res  = 2
      else if (associated( dt, refType )) then; call visit_ref( ref(i) )
      else
        ctgt = C_NULL_PTR
        res  = 0
      end if
    end subroutine
  end function


!_PROC_EXPORT(item_resolve_type)
  function item_resolve_type( ref_, item_ ) result(res)
    use impl_item__, only: TypeInfo_t, Ref_t, Item_t, type_of, temporary_ref
    use impl_item__, only: content_type, ref, item, void_type
    use iso_c_binding
    implicit none
    type(Ref_t),  optional, target :: ref_
    type(Item_t), optional, target :: item_
    type(TypeInfo_t),      pointer :: res
    type(TypeInfo_t),      pointer :: refType, itemType

    refType  => type_of(temporary_ref)
    itemType => type_of(item_)

    if     (present(ref_))  then; call visit_ref( ref_ )
    elseif (present(item_)) then; call visit_item( item_ )
                            else; res => void_type()
    end if

    contains

    recursive &
    subroutine visit_ref( r )
      type(Ref_t) :: r

      res => content_type( r )
      if     (associated( res, refType ))  then; call visit_ref( ref(r) )
      elseif (associated( res, itemType )) then; call visit_item( item(r) )
      end if
    end subroutine

    recursive &
    subroutine visit_item( i )
      type(Item_t) :: i

      res => content_type( i )
      if (associated( res, refType )) then; call visit_ref( ref(i) )
      end if
    end subroutine
  end function



