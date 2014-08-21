
#include "adt/ref_status.fpp"

module base_string
  use iso_c_binding
  implicit none
  private
  
  type, public :: BaseString_t
    _RefStatus                              :: refstat = _ref_HardLent
    integer*4                               :: len     = 0
    character(len=1), dimension(:), pointer :: ptr     => null()
  end type


  type, public :: Attribute_t
    private
    integer*1 :: val = 0
  end type


  type(BaseString_t), parameter :: temporary_string = BaseString_t( _ref_WeakLent, 0, null() )
  type(BaseString_t), parameter :: permanent_string = BaseString_t( _ref_WeakLent, 0, null() )
  type(Attribute_t),  parameter :: attrib_volatile  = Attribute_t(0)
  type(Attribute_t),  parameter :: attrib_permanent = Attribute_t(1)

  ! interface definitions

  interface initialize    ; module procedure bs_init_by_proto, bs_init_by_cs, bs_init_by_buf ; end interface
  interface assign        ; module procedure bs_assign_cs, bs_assign_bs, bs_assign_buf       ; end interface
  interface delete        ; module procedure bs_delete                                       ; end interface
  interface ptr           ; module procedure bs_ptr                                          ; end interface
  interface c_void_ptr    ; module procedure bs_cptr                                         ; end interface
  interface set_attribute ; module procedure bs_set_attribute                                ; end interface
  interface len_ref       ; module procedure bs_len_ref                                      ; end interface
  interface release_weak  ; module procedure bs_release_weak                                 ; end interface
  

  ! interface visibility

  public :: temporary_string, permanent_string
  public :: attrib_volatile, attrib_permanent
  public :: initialize
  public :: assign
  public :: delete
  public :: ptr, c_void_ptr
  public :: set_attribute
  public :: len_ref
  public :: release_weak


contains


  subroutine bs_init_by_proto( bs, proto )
    type(BaseString_t)           :: bs
    type(BaseString_t), optional :: proto

    bs%ptr => null()
    bs%len =  0
    if (present(proto)) then; _ref_init( bs%refstat, _ref_hardness(proto%refstat) )
                        else; bs%refstat = _ref_HardLent
    end if
  end subroutine


  subroutine bs_init_by_cs( bs, cs )
    type(BaseString_t)              :: bs
    character(len=*),    intent(in) :: cs
    character(len=len(cs)), pointer :: tgt

    bs%len = len(cs)
    if (bs%len > 0) then
      bs%refstat = _ref_WeakMine
      allocate( bs%ptr(bs%len) )
      call c_f_pointer( c_loc(bs%ptr(1)), tgt )
      tgt = cs
    end if
  end subroutine

  
  subroutine bs_init_by_buf( bs, buf )
    type(BaseString_t)                         :: bs
    character(len=1), dimension(:), intent(in) :: buf

    bs%len = size(buf)
    if (bs%len > 0) then
      bs%refstat = _ref_WeakMine
      allocate( bs%ptr(bs%len) )
      bs%ptr = buf
    end if
  end subroutine


  subroutine bs_set_attribute( bs, attr )
    type(BaseString_t)            :: bs
    type(Attribute_t), intent(in) :: attr
    _ref_setHard( bs%refstat, attr%val )
  end subroutine


  subroutine bs_release_weak( bs )
    type(BaseString_t) :: bs

    if (_ref_isWeakMine( bs%refstat )) &
      deallocate( bs%ptr )
  end subroutine


  subroutine bs_delete( bs )
    type(BaseString_t) :: bs

    bs%len = 0
    if (_ref_isMine( bs%refstat )) then
      deallocate( bs%ptr )
      _ref_setMine( bs%refstat, 0 )
    else
      bs%ptr => null()
    end if
  end subroutine


  function bs_ptr( bs ) result(res)
    type(BaseString_t), intent(in) :: bs
    character(len=bs%len), pointer :: res

    if (associated(bs%ptr) .and. bs%len > 0) then
      call c_f_pointer( c_loc(bs%ptr(1)), res )
    else
      res => null()
    end if
  end function


  function bs_cptr( bs ) result(res)
    type(BaseString_t) :: bs
    type(c_ptr)        :: res

    res = C_NULL_PTR
    if (_ref_isWeakMine( bs%refstat )) then
      deallocate( bs%ptr )
    else if (associated(bs%ptr) .and. bs%len > 0) then
      res = c_loc(bs%ptr(1))
    end if
  end function


  pure &
  function bs_len_ref( bs ) result(res)
    type(BaseString_t), intent(in) :: bs
    integer                        :: res

    if (_ref_isWeakMine(bs%refstat)) then; res = 0
                                     else; res = bs%len
    end if
  end function
  

  subroutine bs_assign_cs( bs, cs )
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


  subroutine bs_assign_bs( lhs, rhs )
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

