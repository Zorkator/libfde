
#include "ref_status.fpp"

module base_string
  use iso_c_binding
  implicit none
  
  type, public :: BaseString_t
    _RefStatus                              :: refstat = _ref_HardLent
    integer*4                               :: len     = 0
    character(len=1), dimension(:), pointer :: ptr     => null()
  end type

  interface ptr ; module procedure bs_ptr ; end interface

  contains

  subroutine bs_init( rs )
    type(BaseString_t) :: rs

    rs%ptr     => null()
    rs%len     =  0
    rs%refstat = _ref_HardLent
  end subroutine


  subroutine bs_set_hardness( rs, val )
    type(BaseString_t) :: rs
    integer*1          :: val
    _ref_setHard( rs%refstat, val )
  end subroutine


  subroutine bs_init_by_cs( rs, cs )
    type(BaseString_t)              :: rs
    character(len=*),    intent(in) :: cs
    character(len=len(cs)), pointer :: tgt

    rs%len = len(cs)
    if (rs%len > 0) then
      rs%refstat = _ref_WeakMine
      allocate( rs%ptr(rs%len) )
      call c_f_pointer( c_loc(rs%ptr(1)), tgt )
      tgt = cs
    end if
  end subroutine

  
  subroutine bs_init_by_buf( rs, buf )
    type(BaseString_t)                         :: rs
    character(len=1), dimension(:), intent(in) :: buf

    rs%len = size(buf)
    if (rs%len > 0) then
      rs%refstat = _ref_WeakMine
      allocate( rs%ptr(rs%len) )
      rs%ptr = buf
    end if
  end subroutine


  subroutine bs_release_weak( rs )
    type(BaseString_t) :: rs

    if (_ref_isWeakMine( rs%refstat )) &
      deallocate( rs%ptr )
  end subroutine


  subroutine bs_delete( rs )
    type(BaseString_t) :: rs

    rs%len = 0
    if (_ref_isMine( rs%refstat )) then
      deallocate( rs%ptr )
      _ref_setMine( rs%refstat, 0 )
    else
      rs%ptr => null()
    end if
  end subroutine


  function bs_ptr( rs ) result(res)
    type(BaseString_t), intent(in) :: rs
    character(len=rs%len), pointer :: res

    if (associated(rs%ptr) .and. rs%len > 0) then
      call c_f_pointer( c_loc(rs%ptr(1)), res )
    else
      res => null()
    end if
  end function


  function bs_cptr( rs ) result(res)
    type(BaseString_t) :: rs
    type(c_ptr)        :: res

    res = C_NULL_PTR
    if (_ref_isWeakMine( rs%refstat )) then
      deallocate( rs%ptr )
    else if (associated(rs%ptr) .and. rs%len > 0) then
      res = c_loc(rs%ptr(1))
    end if
  end function


  pure function bs_ref_len( rs ) result(res)
    type(BaseString_t), intent(in) :: rs
    integer                        :: res

    if (_ref_isWeakMine(rs%refstat)) then; res = 0
                                     else; res = rs%len
    end if
  end function
  

  subroutine bs_assign_cs( rs, cs )
    type(BaseString_t)              :: rs
    character(len=*),    intent(in) :: cs
    character(len=len(cs)), pointer :: tgt

    rs%len = len(cs)
    if (rs%len == 0) return !< nothing to do

    if (_ref_isMine( rs%refstat )) then
      ! it's my buffer - if it's large enough ...
      if (rs%len <= size(rs%ptr)) goto 20 !< ... just copy new content
      deallocate( rs%ptr )                !< otherwise: dealloc, allocate, copy
    else
      ! it's not my buffer
      rs%ptr => null() !< make sure pointer is null before allocating it!
    end if

    ! update string buffer and content ...
    10  allocate( rs%ptr(rs%len) )
        _ref_setMine( rs%refstat, 1 )
    20  call c_f_pointer( c_loc(rs%ptr(1)), tgt )
        tgt(:rs%len) = cs
  end subroutine


  subroutine bs_assign_buf( lhs, rhs )
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


  subroutine bs_assign_rs( lhs, rhs )
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
        call bs_assign_cs( lhs, ptr(rhs) ) !< use ptr interface to get the right length!
      end if
    end if
  end subroutine

end module

