
!#include "ref_stat.fpp"

# define _ref_clear       b'00'
# define _ref_mine        b'01'
# define _ref_hard        b'10'

# define _ref_soft_lent   b'00'
# define _ref_soft_mine   b'01'
# define _ref_hard_lent   b'10'
# define _ref_hard_mine   b'11'

# define _ref_stat(s)     ior(s%hard, s%mine)

# define _set_mine(s)     s%mine = _ref_mine
# define _clr_mine(s)     s%mine = _ref_clear
# define _set_hard(s)     s%hard = _ref_hard
# define _clr_hard(s)     s%hard = _ref_clear


module dynamic_string
  use iso_c_binding
  implicit none

  type, public :: DynamicString
    private
    character(len=1), dimension(:), pointer :: ptr  => null()
    integer*4                               :: len  = 0
    integer*1                               :: hard = _ref_hard
    integer*1                               :: mine = _ref_clear
  end type

  interface DynamicString; module procedure ds_from_cs ; end interface
  interface str          ; module procedure ds_str     ; end interface
  interface len          ; module procedure ds_len     ; end interface
  interface len_trim     ; module procedure ds_len_trim; end interface
  interface delete       ; module procedure ds_delete  ; end interface

  interface assignment(=);
    module procedure ds_assign_from_cs, ds_assign_to_cs, ds_assign_ds
  end interface

  contains

  function ds_from_cs( cs ) result(ds)
    character(len=*),    intent(in) :: cs
    type (DynamicString)            :: ds
    character(len=len(cs)), pointer :: ptr

    _clr_hard(ds)
    _set_mine(ds)
    ds%len = len(cs) 
    allocate( ds%ptr(ds%len) )
    call c_f_pointer( c_loc(ds%ptr(1)), ptr )
    ptr = cs
  end function


  function ds_str( ds ) result(res)
    type (DynamicString), intent(in) :: ds
    character(len=ds%len),   pointer :: res

    select case (associated(ds%ptr))
      case (.true.) ; call c_f_pointer( c_loc(ds%ptr(1)), res )
      case (.false.); res => null()
    end select
  end function


  pure function ds_len( ds ) result(length)
    type (DynamicString), intent(in) :: ds
    integer                          :: length
    length = ds%len
  end function
  

  function ds_len_trim( ds ) result(length)
    type (DynamicString), intent(in) :: ds
    integer                          :: length

    select case (associated(ds%ptr))
      case (.true.); length = len_trim(str(ds))
      case default;  length = 0
    end select
  end function


  subroutine ds_assign_from_cs( ds, cs )
    type (DynamicString), intent(inout) :: ds
    character(len=*),        intent(in) :: cs
    character(len=len(cs)),     pointer :: ptr
    integer*4                           :: l

    l = len(cs)
    if (ds%mine > 0) then
      select case (l > size(ds%ptr))
        case (.true.) ; goto 10 !< dealloc, allocate, copy
        case (.false.); goto 30 !< copy
      end select
    end if
    ds%ptr => null()
    goto 20 !< allocate, copy

    10 deallocate( ds%ptr )
    20 allocate( ds%ptr(l) )
       _set_mine( ds )
    30 call c_f_pointer( c_loc(ds%ptr(1)), ptr )
       ptr    = cs
       ds%len = l
  end subroutine


  subroutine ds_assign_to_cs( cs, ds )
    character(len=*),    intent(out) :: cs
    type (DynamicString), intent(in) :: ds

    cs = str(ds)
  end subroutine


  subroutine ds_assign_ds( lhs, rhs )
    type (DynamicString), intent(inout) :: lhs
    type (DynamicString),    intent(in) :: rhs

    if (.not. associated(lhs%ptr, rhs%ptr)) then
      ! clear ptr as it's going to get re-assigned
      select case (lhs%mine .ne. 0)
        case (.true.) ; deallocate(lhs%ptr)
        case (.false.); lhs%ptr => null()
      end select

      ! take over string length
      lhs%len = rhs%len

      ! assignment from null-string?
      if      (.not. associated(rhs%ptr)) then
        ! if lhs%mine
        !   dealloc( lhs%ptr )
        _clr_mine( lhs )

      ! assigning from a soft/mine rhs
      else if (_ref_stat(rhs) == _ref_soft_mine) then
        ! take pointer and mark as mine
        !if lhs%mine
        !   dealloc( lhs%ptr )
        lhs%ptr => rhs%ptr
        _set_mine( lhs )

      ! assigning to a hard string
      else if (lhs%hard .ne. 0) then
        ! a hard lhs always get's it's own copy
        ! lhs = rhs%ptr(:rhs%len)
        allocate( lhs%ptr(rhs%len) )
        lhs%ptr = rhs%ptr(:rhs%len) !< caution: don't copy too much!
        _set_mine( lhs )

      ! assigning to a soft string
      else
        ! take the pointer and mark it as NOT mine
        lhs%ptr => rhs%ptr
        _clr_mine( lhs )

      end if
    end if
  end subroutine


  subroutine ds_delete( ds )
    type (DynamicString) :: ds
    
    if (ds%mine > 0) then
      deallocate( ds%ptr )
      _clr_mine(ds)
    else
      ds%ptr => null()
    end if
  end subroutine

end module


#ifdef TEST

program testinger
  use dynamic_string
  implicit none

  type (DynamicString) :: ds, ds2, ds3
  type (DynamicString) :: strings(10)
  integer :: i, idx, jdx

  ds = DynamicString("testinger string")
  print *, str(ds), len(ds)
  print *, str(ds2), len(ds2)
  ds2 = ds
  ds2 = "short"
  print *, str(ds2), len(ds2)
  ds  = ds
  ds2 = ds3
  ds  = ds2

  ds3 = "blubbinger"
  ds2 = ds3

  do i = 1, 10
    idx = mod( i,   size(strings) ) + 1
    jdx = mod( i+1, size(strings) ) + 1

    if (iand( i, 1 ) > 0) then
      strings(idx) = "bla & text"
      strings(jdx) = strings(idx)
    else
      strings(jdx) = DynamicString("voll der testinger string")
      strings(idx) = "dingsinger"
    end if
  end do

  do i = 1, size(strings)
    call delete( strings(i) )
  end do

  call delete( ds )
  call delete( ds2 )
  call delete( ds3 )
end

#endif

