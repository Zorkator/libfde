
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
  private

  type, public :: DynamicString
    private
    character(len=1), dimension(:), pointer :: ptr  => null()
    integer*4                               :: len  = 0
    integer*1                               :: hard = _ref_hard
    integer*1                               :: mine = _ref_clear
  end type

  type :: Attribute
    private
    integer*1 :: val = 0
  end type

  type (Attribute), parameter :: attrib_permanent = Attribute(0)
  type (Attribute), parameter :: attrib_volatile  = Attribute(1)


  ! declare public interfaces 
  public :: attrib_permanent, attrib_volatile

  public :: ref, str, cptr
  public :: char
  public :: delete

  public :: adjustl
  public :: adjustr
  public :: iachar
  public :: ichar
  public :: index
  public :: len
  public :: len_trim
  public :: lge
  public :: lgt
  public :: lle
  public :: llt
  public :: assignment(=)
  public :: operator(//)
  public :: operator(==)
  public :: operator(/=)
  public :: operator(<)
  public :: operator(<=)
  public :: operator(>)
  public :: operator(>=)


  ! interface definitions

  interface DynamicString; module procedure ds_from_cs, ds_from_buf         ; end interface
  interface ptr          ; module procedure ds_ptr                          ; end interface
  interface ref          ; module procedure ds_ref                          ; end interface
  interface cptr         ; module procedure ds_cptr                         ; end interface
  interface str          ; module procedure ds_str                          ; end interface
  interface char         ; module procedure ds_char, ds_char_l              ; end interface
  interface delete       ; module procedure ds_delete                       ; end interface

  interface adjustl      ; module procedure ds_adjustl                      ; end interface
  interface adjustr      ; module procedure ds_adjustr                      ; end interface
  interface iachar       ; module procedure ds_iachar                       ; end interface
  interface ichar        ; module procedure ds_ichar                        ; end interface
  interface index        ; module procedure ds_idx_cs, cs_idx_ds, ds_idx_ds ; end interface
  interface len          ; module procedure ds_len                          ; end interface
  interface len_trim     ; module procedure ds_len_trim                     ; end interface
  interface lge          ; module procedure ds_lge_cs, cs_lge_ds, ds_lge_ds ; end interface
  interface lgt          ; module procedure ds_lgt_cs, cs_lgt_ds, ds_lgt_ds ; end interface
  interface lle          ; module procedure ds_lle_cs, cs_lle_ds, ds_lle_ds ; end interface
  interface llt          ; module procedure ds_llt_cs, cs_llt_ds, ds_llt_ds ; end interface


  ! assignment and operators

  interface assignment(=)
    module procedure ds_assign_cs, cs_assign_ds, ds_assign_ds, ds_assign_buf, ds_assign_attrib
  end interface

  interface operator(//)
    module procedure ds_concat_cs, cs_concat_ds, ds_concat_ds
  end interface

  ! comparison operators

  interface operator(==)
    module procedure ds_eq_cs, cs_eq_ds, ds_eq_ds
  end interface

  interface operator(/=)
    module procedure ds_ne_cs, cs_ne_ds, ds_ne_ds
  end interface

  interface operator(<)
    module procedure ds_lt_cs, cs_lt_ds, ds_lt_ds
  end interface

  interface operator(<=)
    module procedure ds_le_cs, cs_le_ds, ds_le_ds
  end interface

  interface operator(>)
    module procedure ds_gt_cs, cs_gt_ds, ds_gt_ds
  end interface

  interface operator(>=)
    module procedure ds_ge_cs, cs_ge_ds, ds_ge_ds
  end interface


!-----------------
  contains
!-----------------


  ! DynamicString
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

  function ds_from_buf( buf ) result(ds)
    character(len=1), dimension(:), intent(in) :: buf
    type (DynamicString)                       :: ds

    _clr_hard(ds)
    if (size(buf) > 0) then
      ds%len = size(buf)
      _set_mine(ds)
      allocate( ds%ptr(ds%len) )
      ds%ptr = buf
    end if
  end function


  subroutine ds_release_weak( ds )
    type (DynamicString) :: ds
    if (_ref_stat(ds) == _ref_soft_mine) &
      deallocate( ds%ptr )
  end subroutine


  ! ptr
  function ds_ptr( ds ) result(res)
    type (DynamicString), intent(in) :: ds
    character(len=ds%len),   pointer :: res

    res => null()
    if (associated(ds%ptr)) &
      call c_f_pointer( c_loc(ds%ptr(1)), res )
  end function


  ! ref
  function ds_ref( ds ) result(res)
    type (DynamicString)                :: ds
    character(len=ref_len(ds)), pointer :: res

    res => null()
    if (_ref_stat(ds) == _ref_soft_mine) then
      deallocate( ds%ptr )
    else if (associated(ds%ptr)) then
      call c_f_pointer( c_loc(ds%ptr(1)), res )
    end if
  end function


  ! c_loc
  function ds_cptr( ds ) result(res)
    type (DynamicString) :: ds
    type (c_ptr)         :: res

    res = C_NULL_PTR
    if (_ref_stat(ds) == _ref_soft_mine) then
      deallocate( ds%ptr )
    else if (associated(ds%ptr)) then
      res = c_loc(ds%ptr(1))
    end if
  end function


  ! str
  function ds_str( ds ) result(res)
    type (DynamicString)  :: ds
    character(len=ds%len) :: res
    integer               :: idx

    res = ptr(ds)
    call ds_release_weak( ds )
  end function


  ! char
  function ds_char( ds ) result(res)
    type (DynamicString) :: ds
    character(len=1)     :: res(ds%len)

    res = ds%ptr(:ds%len)
    call ds_release_weak( ds )
  end function

  ! char - fixed length
  function ds_char_l( ds, length ) result(res)
    type (DynamicString) :: ds
    integer,  intent(in) :: length
    character(len=1)     :: res(length)
    integer              :: idx, limit

    limit = min(ds%len, length)
    res(1:limit)        = ds%ptr(:limit)
    res(limit+1:length) = ' '
    call ds_release_weak( ds )
  end function
  

  ! delete
  subroutine ds_delete( ds )
    type (DynamicString) :: ds
    
    ds%len = 0
    if (ds%mine > 0) then
      deallocate( ds%ptr )
      _clr_mine(ds)
    else
      ds%ptr => null()
    end if
  end subroutine


  ! assignments

  subroutine ds_assign_cs( ds, cs )
    type (DynamicString), intent(inout) :: ds
    character(len=*),        intent(in) :: cs
    character(len=len(cs)),     pointer :: ptr

    ds%len = len(cs)
    if (ds%mine > 0) then
      ! it's my buffer - reuse it if possible
      select case (ds%len > size(ds%ptr)) !< need bigger block?
        case (.true.) ; goto 10 !< dealloc, allocate, copy
        case (.false.); goto 30 !< copy
      end select
    else
      ! it's not my buffer
      ds%ptr => null() !< make sure pointer is null before allocating it!
      goto 20          !< allocate, copy
    end if

    ! update string buffer and content ...
    10 deallocate( ds%ptr )
    20 allocate( ds%ptr(ds%len) )
       _set_mine( ds )
    30 call c_f_pointer( c_loc(ds%ptr(1)), ptr )
       ptr(:ds%len) = cs
  end subroutine


  subroutine cs_assign_ds( cs, ds )
    character(len=*),    intent(out) :: cs
    type (DynamicString), intent(in) :: ds
    cs = ptr(ds)
    call ds_release_weak( ds )
  end subroutine


  subroutine ds_assign_ds( lhs, rhs )
    type (DynamicString), intent(inout) :: lhs
    type (DynamicString),    intent(in) :: rhs

    ! prevent self assignment ...
    if (.not. associated(lhs%ptr, rhs%ptr)) then
      ! assigning from a soft/mine rhs
      if (_ref_stat(rhs) == _ref_soft_mine) then
        if (lhs%mine > 0) &
          deallocate( lhs%ptr )

        ! take length, pointer and mark as mine
        lhs%ptr => rhs%ptr
        lhs%len = rhs%len
        _set_mine( lhs )

      ! assigning from hard rhs
      else
        lhs = ptr(rhs) !< this works fine for assigning null strings!
      end if
    end if
  end subroutine


  subroutine ds_assign_buf( lhs, rhs )
    type (DynamicString),        intent(inout) :: lhs
    character(len=1), dimension(:), intent(in) :: rhs

    lhs%len = size(rhs)
    if (lhs%mine > 0) then
      ! it's my buffer - reuse it if possible
      select case (lhs%len > size(lhs%ptr)) !< need bigger block?
        case (.true.) ; goto 10 !< dealloc, allocate, copy
        case (.false.); goto 30 !< copy
      end select
    end if

    ! update string buffer and content ...
    10 deallocate( lhs%ptr )
    20 allocate( lhs%ptr(lhs%len) )
       _set_mine( lhs )
    30 lhs%ptr(:lhs%len) = rhs
  end subroutine


  subroutine ds_assign_attrib( lhs, rhs )
    type (DynamicString),  intent(inout) :: lhs
    type (Attribute),         intent(in) :: rhs
    select case (rhs%val)
      case (0); _set_hard(lhs)
      case (1); _clr_hard(lhs)
    end select
  end subroutine


!##################################

  ! adjustl
  function ds_adjustl( ds ) result(res)
    type (DynamicString), intent(in) :: ds
    character(len=ds%len)            :: res
    res = adjustl(ptr(ds))
    call ds_release_weak( ds )
  end function

  ! adjustr
  function ds_adjustr( ds ) result(res)
    type (DynamicString), intent(in) :: ds
    character(len=ds%len)            :: res
    res = adjustr(ptr(ds))
    call ds_release_weak( ds )
  end function


  ! iachar
  function ds_iachar( ds ) result(res)
    type (DynamicString), intent(in) :: ds
    integer                          :: res(ds%len)
    res = iachar(ds%ptr(:ds%len))
    call ds_release_weak( ds )
  end function

  
  ! ichar
  function ds_ichar( ds ) result(res)
    type (DynamicString), intent(in) :: ds
    integer                          :: res(ds%len)
    res = ichar(ds%ptr(:ds%len))
    call ds_release_weak( ds )
  end function


  ! index
  function ds_idx_cs( strg, sub, back ) result(res)
    type (DynamicString), intent(in) :: strg
    character(len=*),     intent(in) :: sub
    logical,    intent(in), optional :: back
    integer                          :: res
    res = index(ptr(strg), sub, back )
    call ds_release_weak( strg )
  end function

  function cs_idx_ds( strg, sub, back ) result(res)
    character(len=*),     intent(in) :: strg
    type (DynamicString), intent(in) :: sub
    logical,    intent(in), optional :: back
    integer                          :: res
    res = index(strg, ptr(sub), back )
    call ds_release_weak( sub )
  end function

  function ds_idx_ds( strg, sub, back ) result(res)
    type (DynamicString), intent(in) :: strg
    type (DynamicString), intent(in) :: sub
    logical,    intent(in), optional :: back
    integer                          :: res
    res = index(ptr(strg), ptr(sub), back )
    call ds_release_weak( strg )
    call ds_release_weak( sub )
  end function


  ! len
  function ds_len( ds ) result(length)
    type (DynamicString), intent(in) :: ds
    integer                          :: length
    length = ds%len
    call ds_release_weak( ds )
  end function

  pure function ref_len( ds ) result(length)
    type (DynamicString), intent(in) :: ds
    integer                          :: length
    length = merge( 0, ds%len, _ref_stat(ds) == _ref_soft_mine )
  end function
  

  ! len_trim
  function ds_len_trim( ds ) result(length)
    type (DynamicString), intent(in) :: ds
    integer                          :: length
    length = len_trim(ptr(ds))
    call ds_release_weak( ds )
  end function


  ! lge
  function ds_lge_cs( strgA, strgB ) result(res)
    type (DynamicString), intent(in) :: strgA
    character(len=*),     intent(in) :: strgB
    logical                          :: res
    res = lge(ptr(strgA), strgB)
    call ds_release_weak( strgA )
  end function

  function cs_lge_ds( strgA, strgB ) result(res)
    character(len=*),     intent(in) :: strgA
    type (DynamicString), intent(in) :: strgB
    logical                          :: res
    res = lge(strgA, ptr(strgB))
    call ds_release_weak( strgB )
  end function

  function ds_lge_ds( strgA, strgB ) result(res)
    type (DynamicString), intent(in) :: strgA
    type (DynamicString), intent(in) :: strgB
    logical                          :: res
    res = lge(ptr(strgA), ptr(strgB))
    call ds_release_weak( strgA )
    call ds_release_weak( strgB )
  end function


  ! lgt
  function ds_lgt_cs( strgA, strgB ) result(res)
    type (DynamicString), intent(in) :: strgA
    character(len=*),     intent(in) :: strgB
    logical                          :: res
    res = lgt(ptr(strgA), strgB)
    call ds_release_weak( strgA )
  end function

  function cs_lgt_ds( strgA, strgB ) result(res)
    character(len=*),     intent(in) :: strgA
    type (DynamicString), intent(in) :: strgB
    logical                          :: res
    res = lgt(strgA, ptr(strgB))
    call ds_release_weak( strgB )
  end function

  function ds_lgt_ds( strgA, strgB ) result(res)
    type (DynamicString), intent(in) :: strgA
    type (DynamicString), intent(in) :: strgB
    logical                          :: res
    res = lgt(ptr(strgA), ptr(strgB))
    call ds_release_weak( strgA )
    call ds_release_weak( strgB )
  end function


  ! lle
  function ds_lle_cs( strgA, strgB ) result(res)
    type (DynamicString), intent(in) :: strgA
    character(len=*),     intent(in) :: strgB
    logical                          :: res
    res = lle(ptr(strgA), strgB)
    call ds_release_weak( strgA )
  end function

  function cs_lle_ds( strgA, strgB ) result(res)
    character(len=*),     intent(in) :: strgA
    type (DynamicString), intent(in) :: strgB
    logical                          :: res
    res = lle(strgA, ptr(strgB))
    call ds_release_weak( strgB )
  end function

  function ds_lle_ds( strgA, strgB ) result(res)
    type (DynamicString), intent(in) :: strgA
    type (DynamicString), intent(in) :: strgB
    logical                          :: res
    res = lle(ptr(strgA), ptr(strgB))
    call ds_release_weak( strgA )
    call ds_release_weak( strgB )
  end function


  ! llt
  function ds_llt_cs( strgA, strgB ) result(res)
    type (DynamicString), intent(in) :: strgA
    character(len=*),     intent(in) :: strgB
    logical                          :: res
    res = llt(ptr(strgA), strgB)
    call ds_release_weak( strgA )
  end function

  function cs_llt_ds( strgA, strgB ) result(res)
    character(len=*),     intent(in) :: strgA
    type (DynamicString), intent(in) :: strgB
    logical                          :: res
    res = llt(strgA, ptr(strgB))
    call ds_release_weak( strgB )
  end function

  function ds_llt_ds( strgA, strgB ) result(res)
    type (DynamicString), intent(in) :: strgA
    type (DynamicString), intent(in) :: strgB
    logical                          :: res
    res = llt(ptr(strgA), ptr(strgB))
    call ds_release_weak( strgA )
    call ds_release_weak( strgB )
  end function


  ! concatenation

  function ds_concat_cs( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    character(len=*),     intent(in) :: rhs
    character(len=lhs%len+len(rhs)) :: res
    res = ptr(lhs) // rhs
    call ds_release_weak( lhs )
  end function

  function cs_concat_ds( lhs, rhs ) result(res)
    character(len=*),     intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    character(len=len(lhs)+rhs%len) :: res
    res = lhs // ptr(rhs)
    call ds_release_weak( rhs )
  end function

  function ds_concat_ds( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    character(len=lhs%len+rhs%len) :: res
    res = ptr(lhs) // ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

  ! equality

  function ds_eq_cs( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    character(len=*),     intent(in) :: rhs
    logical                          :: res
    res = ptr(lhs) == rhs
    call ds_release_weak( lhs )
  end function

  function cs_eq_ds( lhs, rhs ) result(res)
    character(len=*),     intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    logical                          :: res
    res = lhs == ptr(rhs)
    call ds_release_weak( rhs )
  end function

  function ds_eq_ds( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    logical                          :: res
    res = ptr(lhs) == ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

  ! inequality

  function ds_ne_cs( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    character(len=*),     intent(in) :: rhs
    logical                          :: res
    res = ptr(lhs) /= rhs
    call ds_release_weak( lhs )
  end function

  function cs_ne_ds( lhs, rhs ) result(res)
    character(len=*),     intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    logical                          :: res
    res = lhs /= ptr(rhs)
    call ds_release_weak( rhs )
  end function

  function ds_ne_ds( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    logical                          :: res
    res = ptr(lhs) /= ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

  ! lower than

  function ds_lt_cs( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    character(len=*),     intent(in) :: rhs
    logical                          :: res
    res = ptr(lhs) < rhs
    call ds_release_weak( lhs )
  end function

  function cs_lt_ds( lhs, rhs ) result(res)
    character(len=*),     intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    logical                          :: res
    res = lhs < ptr(rhs)
    call ds_release_weak( rhs )
  end function

  function ds_lt_ds( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    logical                          :: res
    res = ptr(lhs) < ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

  ! lower equal

  function ds_le_cs( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    character(len=*),     intent(in) :: rhs
    logical                          :: res
    res = ptr(lhs) <= rhs
    call ds_release_weak( lhs )
  end function

  function cs_le_ds( lhs, rhs ) result(res)
    character(len=*),     intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    logical                          :: res
    res = lhs <= ptr(rhs)
    call ds_release_weak( rhs )
  end function

  function ds_le_ds( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    logical                          :: res
    res = ptr(lhs) <= ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

  ! greater than

  function ds_gt_cs( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    character(len=*),     intent(in) :: rhs
    logical                          :: res
    res = ptr(lhs) > rhs
    call ds_release_weak( lhs )
  end function

  function cs_gt_ds( lhs, rhs ) result(res)
    character(len=*),     intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    logical                          :: res
    res = lhs > ptr(rhs)
    call ds_release_weak( rhs )
  end function


  function ds_gt_ds( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    logical                          :: res
    res = ptr(lhs) > ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

  ! greater equal

  function ds_ge_cs( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    character(len=*),     intent(in) :: rhs
    logical                          :: res
    res = ptr(lhs) >= rhs
    call ds_release_weak( lhs )
  end function

  function cs_ge_ds( lhs, rhs ) result(res)
    character(len=*),     intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    logical                          :: res
    res = lhs >= ptr(rhs)
    call ds_release_weak( rhs )
  end function

  function ds_ge_ds( lhs, rhs ) result(res)
    type (DynamicString), intent(in) :: lhs
    type (DynamicString), intent(in) :: rhs
    logical                          :: res
    res = ptr(lhs) >= ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

end module

!##################################################################################################
#ifdef TEST

program testinger
  use dynamic_string
  implicit none

  type (DynamicString) :: tmp
  type (DynamicString) :: ds, ds2, ds3
  type (DynamicString) :: strings(4)
  character            :: buffer(10), bufferB(10)
  character            :: buffer2(5)

  character(len=32)    :: stringArray(20)
  character(len=10)    :: strMat(10,20)

  integer :: i, idx, jdx

  tmp = attrib_volatile
  tmp = attrib_permanent

  ds = DynamicString("test string")
  print *, ref(ds), len(ds)   !< print string and its length
  print *, ref(ds2), len(ds2) !< print null string and its length
  ds2 = ds                    !< assignment to null string
  ds2 = "short"               !< assignment of shorter character string (no re-allocation!)
  print *, str(ds2), len(ds2) !< print shorter string from original buffer ...
  ds  = ds                    !< self assignment (no effect!)
  ds2 = ds3                   !< assign null string (cleared but no deallocation!)
  print *, str(ds2), len(ds2) !< print cleared string and its length
  ds2 = ds // ds2             !< concat and reassign 

  print *, ds2 // " concat"
  print *, "prefix " // ds2
  print *, ds2 // ds2

  print *, ds3 .eq. DynamicString("foo")
  print *, "bar" == ds3
  print *, ds2 // 'bla' .eq. DynamicString(char(ds3))


  ds2 = "some long test string of more than ten characters ..."
  print *, char(ds2)
  print *, char(ds2, 10)

  ds2 = "    short    "
  print *, char(ds2)
  print *, char(ds2, 10)
  print *, iachar(ds2)
  print *, ichar(ds2)
  print *, "#" // adjustr(ds2) // '#'
  print *, "#" // adjustl(ds2) // '#' 
  print *, "#" // adjustr(DynamicString('text')) // '#'
  print *, "#" // adjustl(DynamicString('       text ')) // '#' // DynamicString('usw')

  buffer  = char(ds2)
  print *, buffer

  print *, str(DynamicString('testinger'))
  ds2 = DynamicString( buffer ) // " appendix"
  ds2 = buffer

  !print *, cptr(ds2)

  print *, lge( buffer, char(ds2) )

  do i = 1, 1000
    idx = mod( i,   size(strings) ) + 1
    jdx = mod( i+7, size(strings) ) + 1

    if (iand( i, 1 ) > 0) then
      strings(jdx) = strings(idx)
      strings(idx) = "another string"
    else
      strings(jdx) = DynamicString("long test string, converted to soft DynamicString")
      strings(idx) = "yet another test string"
    end if
  end do

  do i = 1, size(strings)
    call delete( strings(i) )
  end do

  call delete( ds )
  call delete( ds2 )
  call delete( ds3 )

  call acceptStringArray( stringArray )
  call acceptStringMatrix( strMat )

  contains

  subroutine acceptStringArray( x )
    character(*), dimension(:) :: x
  end subroutine

  subroutine acceptStringMatrix( x )
    character(*), dimension(:,:) :: x
  end subroutine


end

#endif

