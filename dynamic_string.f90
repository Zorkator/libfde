
#include "ref_status.fpp"

module dynamic_string
  use iso_c_binding
  implicit none
  private


  type, public :: DynamicString_t
    private
    _RefStatus                              :: refstat = _ref_HardLent
    integer*4                               :: len     = 0
    character(len=1), dimension(:), pointer :: ptr     => null()
  end type

  type :: Attribute
    private
    integer*1 :: val = 0
  end type

  type(Attribute), parameter :: attrib_volatile  = Attribute(0)
  type(Attribute), parameter :: attrib_permanent = Attribute(1)


  ! interface definitions

  interface DynamicString; module procedure ds_from_cs, ds_from_buf         ; end interface
  interface ptr          ; module procedure ds_ptr                          ; end interface
  interface str          ; module procedure ds_str                          ; end interface
  interface cptr         ; module procedure ds_cptr                         ; end interface
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

  ! declare public interfaces 

  public :: DynamicString
  public :: str, cptr
  public :: char
  public :: delete
  public :: ds_initialize, ds_assign_ds, ds_delete
  public :: attrib_permanent, attrib_volatile

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


  subroutine ds_initialize( ds )
    type(DynamicString_t) :: ds
    
    ds%ptr     => null()
    ds%len     = 0
    ds%refstat = _ref_HardLent
  end subroutine


  ! DynamicString
  function ds_from_cs( cs ) result(ds)
    character(len=*),    intent(in) :: cs
    type(DynamicString_t)           :: ds
    character(len=len(cs)), pointer :: ptr

    ds%len = len(cs)
    if (ds%len > 0) then
      ds%refstat = _ref_WeakMine
      allocate( ds%ptr(ds%len) )
      call c_f_pointer( c_loc(ds%ptr(1)), ptr )
      ptr = cs
    end if
  end function

  function ds_from_buf( buf ) result(ds)
    character(len=1), dimension(:), intent(in) :: buf
    type(DynamicString_t)                      :: ds

    _ref_setHard( ds%refstat, 0 )
    if (size(buf) > 0) then
      ds%len = size(buf)
      _ref_setMine( ds%refstat, 1 )
      allocate( ds%ptr(ds%len) )
      ds%ptr = buf
    end if
  end function


  subroutine ds_release_weak( ds )
    type(DynamicString_t) :: ds
    if (_ref_isWeakMine( ds%refstat )) &
      deallocate( ds%ptr )
  end subroutine


  ! ptr
  function ds_ptr( ds ) result(res)
    type(DynamicString_t), intent(in) :: ds
    character(len=ds%len),    pointer :: res

    res => null()
    if (associated(ds%ptr) .and. ds%len > 0) &
      call c_f_pointer( c_loc(ds%ptr(1)), res )
  end function


  ! str
  function ds_str( ds ) result(res)
    type(DynamicString_t)               :: ds
    character(len=ref_len(ds)), pointer :: res

    res => null()
    if (_ref_isWeakMine( ds%refstat )) then
      deallocate( ds%ptr )
    else if (associated(ds%ptr) .and. ds%len > 0) then
      call c_f_pointer( c_loc(ds%ptr(1)), res )
    end if
  end function


  ! c_loc
  function ds_cptr( ds ) result(res)
    type(DynamicString_t) :: ds
    type(c_ptr)           :: res

    res = C_NULL_PTR
    if (_ref_isWeakMine( ds%refstat )) then
      deallocate( ds%ptr )
    else if (associated(ds%ptr)) then
      res = c_loc(ds%ptr(1))
    end if
  end function


  ! char
  function ds_char( ds ) result(res)
    type(DynamicString_t) :: ds
    character(len=ds%len) :: res

    res = ptr(ds)
    call ds_release_weak( ds )
  end function

  ! char - fixed length
  function ds_char_l( ds, length ) result(res)
    type(DynamicString_t) :: ds
    integer,   intent(in) :: length
    character(len=length) :: res
    integer               :: idx, limit

    limit = min(ds%len, length)
    res(1:limit)        = ptr(ds)
    res(limit+1:length) = ' '
    call ds_release_weak( ds )
  end function
  

  ! delete
  subroutine ds_delete( ds )
    type(DynamicString_t) :: ds
    
    ds%len = 0
    if (_ref_isMine( ds%refstat )) then
      deallocate( ds%ptr )
      _ref_setMine( ds%refstat, 0 )
    else
      ds%ptr => null()
    end if
  end subroutine


  ! assignments

  subroutine ds_assign_cs( lhs, rhs )
    type(DynamicString_t), intent(inout) :: lhs
    character(len=*),         intent(in) :: rhs
    character(len=len(rhs)),     pointer :: ptr

    lhs%len = len(rhs)
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
    20  call c_f_pointer( c_loc(lhs%ptr(1)), ptr )
        ptr(:lhs%len) = rhs
  end subroutine


  subroutine cs_assign_ds( cs, ds )
    character(len=*),     intent(out) :: cs
    type(DynamicString_t), intent(in) :: ds
    cs = ptr(ds)
    call ds_release_weak( ds )
  end subroutine


  subroutine ds_assign_ds( lhs, rhs )
    type(DynamicString_t), intent(inout) :: lhs
    type(DynamicString_t),    intent(in) :: rhs

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
        lhs = ptr(rhs) !< use ptr here to get the right length!
      end if
    end if
  end subroutine


  subroutine ds_assign_buf( lhs, rhs )
    type(DynamicString_t),       intent(inout) :: lhs
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


  subroutine ds_assign_attrib( lhs, rhs )
    type(DynamicString_t),  intent(inout) :: lhs
    type(Attribute),           intent(in) :: rhs
    _ref_setHard( lhs%refstat, rhs%val )
  end subroutine


!##################################

  ! adjustl
  function ds_adjustl( ds ) result(res)
    type(DynamicString_t), intent(in) :: ds
    character(len=ds%len)             :: res
    res = adjustl(ptr(ds))
    call ds_release_weak( ds )
  end function

  ! adjustr
  function ds_adjustr( ds ) result(res)
    type(DynamicString_t), intent(in) :: ds
    character(len=ds%len)             :: res
    res = adjustr(ptr(ds))
    call ds_release_weak( ds )
  end function


  ! iachar
  function ds_iachar( ds ) result(res)
    type(DynamicString_t), intent(in) :: ds
    integer                           :: res(ds%len)
    res = iachar(ds%ptr(:ds%len))
    call ds_release_weak( ds )
  end function

  
  ! ichar
  function ds_ichar( ds ) result(res)
    type(DynamicString_t), intent(in) :: ds
    integer                           :: res(ds%len)
    res = ichar(ds%ptr(:ds%len))
    call ds_release_weak( ds )
  end function


  ! index
  function ds_idx_cs( strg, sub, back ) result(res)
    type(DynamicString_t), intent(in) :: strg
    character(len=*),      intent(in) :: sub
    logical,     intent(in), optional :: back
    integer                           :: res
    res = index(ptr(strg), sub, back )
    call ds_release_weak( strg )
  end function

  function cs_idx_ds( strg, sub, back ) result(res)
    character(len=*),      intent(in) :: strg
    type(DynamicString_t), intent(in) :: sub
    logical,     intent(in), optional :: back
    integer                           :: res
    res = index(strg, ptr(sub), back )
    call ds_release_weak( sub )
  end function

  function ds_idx_ds( strg, sub, back ) result(res)
    type(DynamicString_t), intent(in) :: strg
    type(DynamicString_t), intent(in) :: sub
    logical,     intent(in), optional :: back
    integer                           :: res
    res = index(ptr(strg), ptr(sub), back )
    call ds_release_weak( strg )
    call ds_release_weak( sub )
  end function


  ! len
  function ds_len( ds ) result(length)
    type(DynamicString_t), intent(in) :: ds
    integer                           :: length
    length = ds%len
    call ds_release_weak( ds )
  end function

  pure function ref_len( ds ) result(length)
    type(DynamicString_t), intent(in) :: ds
    integer                           :: length
    if (_ref_isWeakMine(ds%refstat)) then; length = 0
                                     else; length = ds%len
    end if
  end function
  

  ! len_trim
  function ds_len_trim( ds ) result(length)
    type(DynamicString_t), intent(in) :: ds
    integer                           :: length
    length = len_trim(ptr(ds))
    call ds_release_weak( ds )
  end function


  ! lge
  function ds_lge_cs( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    character(len=*),      intent(in) :: strgB
    logical                           :: res
    res = lge(ptr(strgA), strgB)
    call ds_release_weak( strgA )
  end function

  function cs_lge_ds( strgA, strgB ) result(res)
    character(len=*),      intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = lge(strgA, ptr(strgB))
    call ds_release_weak( strgB )
  end function

  function ds_lge_ds( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = lge(ptr(strgA), ptr(strgB))
    call ds_release_weak( strgA )
    call ds_release_weak( strgB )
  end function


  ! lgt
  function ds_lgt_cs( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    character(len=*),      intent(in) :: strgB
    logical                           :: res
    res = lgt(ptr(strgA), strgB)
    call ds_release_weak( strgA )
  end function

  function cs_lgt_ds( strgA, strgB ) result(res)
    character(len=*),      intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = lgt(strgA, ptr(strgB))
    call ds_release_weak( strgB )
  end function

  function ds_lgt_ds( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = lgt(ptr(strgA), ptr(strgB))
    call ds_release_weak( strgA )
    call ds_release_weak( strgB )
  end function


  ! lle
  function ds_lle_cs( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    character(len=*),      intent(in) :: strgB
    logical                           :: res
    res = lle(ptr(strgA), strgB)
    call ds_release_weak( strgA )
  end function

  function cs_lle_ds( strgA, strgB ) result(res)
    character(len=*),      intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = lle(strgA, ptr(strgB))
    call ds_release_weak( strgB )
  end function

  function ds_lle_ds( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = lle(ptr(strgA), ptr(strgB))
    call ds_release_weak( strgA )
    call ds_release_weak( strgB )
  end function


  ! llt
  function ds_llt_cs( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    character(len=*),      intent(in) :: strgB
    logical                           :: res
    res = llt(ptr(strgA), strgB)
    call ds_release_weak( strgA )
  end function

  function cs_llt_ds( strgA, strgB ) result(res)
    character(len=*),      intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = llt(strgA, ptr(strgB))
    call ds_release_weak( strgB )
  end function

  function ds_llt_ds( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = llt(ptr(strgA), ptr(strgB))
    call ds_release_weak( strgA )
    call ds_release_weak( strgB )
  end function


  ! concatenation

  function ds_concat_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    character(len=lhs%len+len(rhs))   :: res
    res = ptr(lhs) // rhs
    call ds_release_weak( lhs )
  end function

  function cs_concat_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    character(len=len(lhs)+rhs%len)   :: res
    res = lhs // ptr(rhs)
    call ds_release_weak( rhs )
  end function

  function ds_concat_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    character(len=lhs%len+rhs%len)    :: res
    res = ptr(lhs) // ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

  ! equality

  function ds_eq_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    logical                           :: res
    res = ptr(lhs) == rhs
    call ds_release_weak( lhs )
  end function

  function cs_eq_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = lhs == ptr(rhs)
    call ds_release_weak( rhs )
  end function

  function ds_eq_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = ptr(lhs) == ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

  ! inequality

  function ds_ne_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    logical                           :: res
    res = ptr(lhs) /= rhs
    call ds_release_weak( lhs )
  end function

  function cs_ne_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = lhs /= ptr(rhs)
    call ds_release_weak( rhs )
  end function

  function ds_ne_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = ptr(lhs) /= ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

  ! lower than

  function ds_lt_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    logical                           :: res
    res = ptr(lhs) < rhs
    call ds_release_weak( lhs )
  end function

  function cs_lt_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = lhs < ptr(rhs)
    call ds_release_weak( rhs )
  end function

  function ds_lt_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = ptr(lhs) < ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

  ! lower equal

  function ds_le_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    logical                           :: res
    res = ptr(lhs) <= rhs
    call ds_release_weak( lhs )
  end function

  function cs_le_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = lhs <= ptr(rhs)
    call ds_release_weak( rhs )
  end function

  function ds_le_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = ptr(lhs) <= ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

  ! greater than

  function ds_gt_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    logical                           :: res
    res = ptr(lhs) > rhs
    call ds_release_weak( lhs )
  end function

  function cs_gt_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = lhs > ptr(rhs)
    call ds_release_weak( rhs )
  end function


  function ds_gt_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = ptr(lhs) > ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

  ! greater equal

  function ds_ge_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    logical                           :: res
    res = ptr(lhs) >= rhs
    call ds_release_weak( lhs )
  end function

  function cs_ge_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = lhs >= ptr(rhs)
    call ds_release_weak( rhs )
  end function

  function ds_ge_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = ptr(lhs) >= ptr(rhs)
    call ds_release_weak( lhs )
    call ds_release_weak( rhs )
  end function

end module

