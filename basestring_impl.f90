
#include "adt/itfUtil.fpp"
#include "adt/ref_status.fpp"


!_PROC_EXPORT(basestring_init_by_basestring_c)
!_ARG_REFERENCE2(bs, proto)
  subroutine basestring_init_by_basestring_c( bs, has_proto, proto )
    use adt_basestring, only: BaseString_t, basestring_assign_buf
    use iso_c_binding
    implicit none
    type(BaseString_t), intent(inout) :: bs
    integer(kind=4),    intent(in)    :: has_proto
    type(BaseString_t), intent(in)    :: proto

    bs%ptr => null()
    bs%len =  0
    if (has_proto /= 0) then
      _ref_init( bs%refstat, _ref_hardness(proto%refstat) )
      if (associated( proto%ptr )) &
        call basestring_assign_buf( bs, proto%ptr )
    else
      bs%refstat = _ref_HardLent
    end if
  end subroutine


!_PROC_EXPORT(basestring_init_by_charstring_c)
!_ARG_REFERENCE1(bs)
  subroutine basestring_init_by_charstring_c( bs, attr, cs )
    use adt_basestring, only: BaseString_t
    use iso_c_binding
    implicit none
    type(BaseString_t)              :: bs
    integer(kind=1),     intent(in) :: attr
    character(len=*),    intent(in) :: cs
    character(len=len(cs)), pointer :: tgt

    bs%len = len(cs)
    if (bs%len > 0) then
      allocate( bs%ptr(bs%len) )
      _ref_initMine( bs%refstat, attr )
      call c_f_pointer( c_loc(bs%ptr(1)), tgt )
      tgt = cs
    else
      bs%ptr => null()
      _ref_init( bs%refstat, attr )
    end if
  end subroutine

  
!_PROC_EXPORT(basestring_init_by_buf)
!_ARG_REFERENCE1(bs)
  subroutine basestring_init_by_buf( bs, attr, buf )
    use adt_basestring, only: BaseString_t
    implicit none
    type(BaseString_t)                         :: bs
    integer(kind=1),                intent(in) :: attr
    character(len=1), dimension(:), intent(in) :: buf

    bs%len = size(buf)
    if (bs%len > 0) then
      allocate( bs%ptr(bs%len) )
      _ref_initMine( bs%refstat, attr )
      bs%ptr = buf
    else
      bs%ptr => null()
      _ref_init( bs%refstat, attr )
    end if
  end subroutine


!_PROC_EXPORT(basestring_set_attribute)
!_ARG_REFERENCE1(bs)
  subroutine basestring_set_attribute( bs, attr )
    use adt_basestring, only: BaseString_t
    implicit none
    type(BaseString_t)          :: bs
    integer(kind=1), intent(in) :: attr
  
    _ref_setHard( bs%refstat, attr )
  end subroutine


!_PROC_EXPORT(basestring_release_weak)
!_ARG_REFERENCE1(bs)
  subroutine basestring_release_weak( bs )
    use adt_basestring, only: BaseString_t
    implicit none
    type(BaseString_t) :: bs
  
    if (_ref_isWeakMine( bs%refstat )) &
      deallocate( bs%ptr )
  end subroutine


!_PROC_EXPORT(basestring_delete_c)
!_ARG_REFERENCE1(bs)
  subroutine basestring_delete_c( bs )
    use adt_basestring, only: BaseString_t
    implicit none
    type(BaseString_t) :: bs

    bs%len = 0
    if (_ref_isMine( bs%refstat )) then
      deallocate( bs%ptr )
      _ref_setMine( bs%refstat, 0 )
    else
      bs%ptr => null()
    end if
  end subroutine


!_PROC_EXPORT(basestring_ptr)
!_ARG_REFERENCE1(bs)
  function basestring_ptr( bs ) result(res)
    use adt_basestring, only: BaseString_t
    use iso_c_binding
    implicit none
    type(BaseString_t), intent(in) :: bs
    character(len=bs%len), pointer :: res

    if (associated(bs%ptr) .and. bs%len > 0) then
      call c_f_pointer( c_loc(bs%ptr(1)), res )
    else
      res => null()
    end if
  end function


!_PROC_EXPORT(basestring_cptr)
!_ARG_REFERENCE1(bs)
  function basestring_cptr( bs ) result(res)
    use adt_basestring, only: BaseString_t
    use iso_c_binding
    implicit none
    type(BaseString_t) :: bs
    type(c_ptr)        :: res

    res = C_NULL_PTR
    if (_ref_isWeakMine( bs%refstat )) then
      deallocate( bs%ptr )
    else if (associated(bs%ptr) .and. bs%len > 0) then
      res = c_loc(bs%ptr(1))
    end if
  end function


!_PROC_EXPORT(basestring_cptr_c)
!_ARG_REFERENCE2(res, bs)
  subroutine basestring_cptr_c( res, bs )
    use adt_basestring, only: BaseString_t
    use iso_c_binding
    implicit none
    type(c_ptr),        intent(inout) :: res
    type(BaseString_t)                :: bs
    integer,                parameter :: stderr = 0
    integer                           :: stat

    res = C_NULL_PTR
    if (_ref_isWeakMine( bs%refstat )) then
# if ADT_DEBUG == ENABLED
      write(stderr,*,iostat=stat) "WARNING: applied basestring_cptr_c on weak reference string!"
# endif
      deallocate( bs%ptr )
    else if (associated(bs%ptr) .and. bs%len > 0) then
      res = c_loc(bs%ptr(1))
    end if
  end subroutine


!_PROC_EXPORT(basestring_memoryref_c)
!_ARG_REFERENCE2(res, bs)
  subroutine basestring_memoryref_c( res, bs )
    use adt_basestring, only: BaseString_t
    use adt_memoryref
    use iso_c_binding
    type(MemoryRef_t),  intent(inout) :: res
    type(BaseString_t), intent(in)    :: bs

    if (associated(bs%ptr) .and. bs%len > 0) then
      res%loc = c_loc(bs%ptr(1))
      res%len = bs%len
    end if
  end subroutine


!_PROC_EXPORT(basestring_len_ref)
!_ARG_REFERENCE1(bs)
  pure &
  function basestring_len_ref( bs ) result(res)
    use adt_basestring, only: BaseString_t
    implicit none
    type(BaseString_t), intent(in) :: bs
    integer(kind=4)                :: res

    if (_ref_isWeakMine(bs%refstat)) then; res = 0
                                     else; res = bs%len
    end if
  end function


!_PROC_EXPORT(basestring_reserve)
!_ARG_REFERENCE1(bs)
  subroutine basestring_reserve( bs, length )
    use adt_basestring, only: BaseString_t, basestring_ptr, basestring_assign_charstring_c
    use iso_c_binding
    implicit none
    type(BaseString_t)     :: bs
    integer(kind=c_size_t) :: length
    
    if (bs%len < length) then
      call basestring_assign_charstring_c( bs, basestring_ptr(bs) // repeat( ' ', length - bs%len ) )
    end if
  end subroutine

  
!_PROC_EXPORT(basestring_trim)
!_ARG_REFERENCE1(bs)
  subroutine basestring_trim( bs )
    use adt_basestring, only: BaseString_t, basestring_ptr
    implicit none
    type(BaseString_t) :: bs

    bs%len = len_trim( basestring_ptr(bs) )
  end subroutine
  

!_PROC_EXPORT(basestring_assign_charstring_c)
!_ARG_REFERENCE1(bs)
  subroutine basestring_assign_charstring_c( bs, cs )
    use adt_basestring, only: BaseString_t
    use iso_c_binding
    implicit none
    type(BaseString_t)              :: bs
    character(len=*),    intent(in) :: cs
    character(len=len(cs)), pointer :: tgt

    bs%len = len(cs)
    if (bs%len == 0) return !< nothing to do

    if (_ref_isMine( bs%refstat )) then
      ! it's my buffer - if it's large enough ...
      if (bs%len <= size(bs%ptr)) goto 20 !< ... just copy new content
      deallocate( bs%ptr )                !< otherwise: dealloc, allocate, copy
    else
      ! it's not my buffer
      bs%ptr => null() !< make sure pointer is null before allocating it!
    end if

    ! update string buffer and content ...
    10  allocate( bs%ptr(bs%len) )
        _ref_setMine( bs%refstat, 1 )
    20  call c_f_pointer( c_loc(bs%ptr(1)), tgt )
        tgt(:bs%len) = cs
  end subroutine


!_PROC_EXPORT(basestring_assign_buf)
!_ARG_REFERENCE2(lhs, rhs)
  subroutine basestring_assign_buf( lhs, rhs )
    use adt_basestring, only: BaseString_t
    implicit none
    type(BaseString_t),          intent(inout) :: lhs
    character(len=1), dimension(:), intent(in) :: rhs

    lhs%len = size(rhs)
    if (lhs%len == 0) return !< nothing to do

    if (_ref_isMine( lhs%refstat )) then
      ! it's my buffer - if it's large enough ...
      if (lhs%len <= size(lhs%ptr)) goto 20 !< ... just copy new content
      deallocate( lhs%ptr )                 !< otherwise: dealloc, allocate, copy
    else
      ! it's not my buffer
      lhs%ptr => null() !< make sure pointer is null before allocating it!
    end if

    ! update string buffer and content ...
    10  allocate( lhs%ptr(lhs%len) )
        _ref_setMine( lhs%refstat, 1 )
    20  lhs%ptr(:lhs%len) = rhs
  end subroutine


!_PROC_EXPORT(basestring_assign_basestring_c)
!_ARG_REFERENCE2(lhs, rhs)
  subroutine basestring_assign_basestring_c( lhs, rhs )
    use adt_basestring, only: BaseString_t, basestring_ptr
    implicit none
    type(BaseString_t), intent(inout) :: lhs
    type(BaseString_t),    intent(in) :: rhs

    ! prevent self assignment ...
    if (.not. associated(lhs%ptr, rhs%ptr)) then
      ! assigning from a soft/mine rhs
      if (_ref_isWeakMine( rhs%refstat )) then
        if (_ref_isMine( lhs%refstat )) &
          deallocate( lhs%ptr )

        ! take length, pointer and mark as mine
        lhs%ptr => rhs%ptr
        lhs%len = rhs%len
        _ref_setMine( lhs%refstat, 1 )

      ! assigning from hard rhs
      else
        call basestring_assign_charstring_c( lhs, basestring_ptr(rhs) ) !< use ptr function to get the right length!
      end if
    end if
  end subroutine


