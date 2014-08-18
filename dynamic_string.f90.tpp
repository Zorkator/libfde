
module dynamic_string
  use iso_c_binding
  use base_string
  use generic_ref
  implicit none
  private


  type, public :: DynamicString_t
    private
    type(BaseString_t) :: str

#   define _refstat(self)   self%str%refstat
#   define _len(self)       self%str%len
#   define _ptr(self)       ptr(self%str)
#   define _array(self)     self%str%ptr(:self%str%len)
#   define _reflen(self)    bs_ref_len( self%str )
  end type


  ! interface definitions

  interface DynamicString; module procedure ds_from_cs, ds_from_buf         ; end interface
  interface str          ; module procedure ds_str                          ; end interface
  interface cptr         ; module procedure ds_cptr                         ; end interface
  interface char         ; module procedure ds_char, ds_char_l              ; end interface
  interface achar        ; module procedure ds_achar, ds_achar_l            ; end interface
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

  !public :: DynamicString <= will be declared public later ...
  public :: str, cptr
  public :: char
  public :: delete
  public :: attrib_permanent, attrib_volatile

  public :: adjustl
  public :: adjustr
  public :: iachar
  public :: ichar
  public :: achar
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

# define _release_weak( ds ) \
    call bs_release_weak( ds%str )


  !_TypeGen_declare_RefType( public, DynamicString, type(DynamicString_t), scalar, \
  !     initProc   = ds_initialize, \
  !     assignProc = ds_assign_ds,  \
  !     deleteProc = ds_delete,     \
  !     cloneMode  = _type )

!-----------------
  contains
!-----------------

  !_TypeGen_implementAll()


  subroutine ds_initialize( ds, hardness )
    type(DynamicString_t) :: ds
    integer               :: hardness
    call bs_init( ds%str, hardness )
  end subroutine


  ! DynamicString
  function ds_from_cs( cs ) result(ds)
    character(len=*),    intent(in) :: cs
    type(DynamicString_t)           :: ds
    call bs_init_by_cs( ds%str, cs )
  end function


  function ds_from_buf( buf ) result(ds)
    character(len=1), dimension(:), intent(in) :: buf
    type(DynamicString_t)                      :: ds
    call bs_init_by_buf( ds%str, buf )
  end function


  ! str
  function ds_str( ds ) result(res)
    type(DynamicString_t)               :: ds
    character(len=_reflen(ds)), pointer :: res
    type(c_ptr)                         :: cptr
    cptr = bs_cptr( ds%str )
    if (c_associated( cptr )) then; call c_f_pointer( cptr, res )
                              else; res => null()
    end if
  end function


  ! cptr
  function ds_cptr( ds ) result(res)
    type(DynamicString_t) :: ds
    type(c_ptr)           :: res
    res = bs_cptr( ds%str )
  end function


  ! char
  function ds_char( ds ) result(res)
    type(DynamicString_t)   :: ds
    character(len=_len(ds)) :: res
    res = _ptr(ds)
    _release_weak( ds )
  end function


  ! char - fixed length
  function ds_char_l( ds, length ) result(res)
    type(DynamicString_t) :: ds
    integer,   intent(in) :: length
    character(len=length) :: res
    integer               :: limit

    limit = min(_len(ds), length)
    res(1:limit)  = _ptr(ds)
    res(limit+1:) = ' '
    _release_weak( ds )
  end function
  

  ! achar
  function ds_achar( ds ) result(res)
    type(DynamicString_t), intent(in) :: ds
    character(len=1)                  :: res(_len(ds))
    res = _array(ds)
    _release_weak( ds )
  end function


  ! achar - fixed length
  function ds_achar_l( ds, length ) result(res)
    type(DynamicString_t), intent(in) :: ds
    integer,   intent(in)             :: length
    character(len=1)                  :: res(length)
    integer                           :: limit

    limit = min(_len(ds), length)
    res(1:limit)  = _array(ds)
    res(limit+1:) = ' '
    _release_weak( ds )
  end function


  ! delete
  subroutine ds_delete( ds )
    type(DynamicString_t) :: ds
    call bs_delete( ds%str )
  end subroutine


  ! assignments

  subroutine ds_assign_cs( lhs, rhs )
    type(DynamicString_t), intent(inout) :: lhs
    character(len=*),         intent(in) :: rhs
    call bs_assign_cs( lhs%str, rhs )
  end subroutine


  subroutine cs_assign_ds( cs, ds )
    character(len=*),     intent(out) :: cs
    type(DynamicString_t), intent(in) :: ds
    cs = _ptr(ds)
    _release_weak( ds )
  end subroutine


  subroutine ds_assign_ds( lhs, rhs )
    type(DynamicString_t), intent(inout) :: lhs
    type(DynamicString_t),    intent(in) :: rhs
    call bs_assign_bs( lhs%str, rhs%str )
  end subroutine


  subroutine ds_assign_buf( lhs, rhs )
    type(DynamicString_t),       intent(inout) :: lhs
    character(len=1), dimension(:), intent(in) :: rhs
    call bs_assign_buf( lhs%str, rhs )
  end subroutine


  subroutine ds_assign_attrib( lhs, rhs )
    type(DynamicString_t),  intent(inout) :: lhs
    type(Attribute_t),         intent(in) :: rhs
    call bs_set_attribute( lhs%str, rhs )
  end subroutine


!##################################

  ! adjustl
  function ds_adjustl( ds ) result(res)
    type(DynamicString_t), intent(in) :: ds
    character(len=_len(ds))           :: res
    res = adjustl(_ptr(ds))
    _release_weak( ds )
  end function

  ! adjustr
  function ds_adjustr( ds ) result(res)
    type(DynamicString_t), intent(in) :: ds
    character(len=_len(ds))           :: res
    res = adjustr(_ptr(ds))
    _release_weak( ds )
  end function


  ! iachar
  function ds_iachar( ds ) result(res)
    type(DynamicString_t), intent(in) :: ds
    integer                           :: res(_len(ds))
    res = iachar(_array(ds))
    _release_weak( ds )
  end function

  
  ! ichar
  function ds_ichar( ds ) result(res)
    type(DynamicString_t), intent(in) :: ds
    integer                           :: res(_len(ds))
    res = ichar(_array(ds))
    _release_weak( ds )
  end function


  ! index
  function ds_idx_cs( strg, sub, back ) result(res)
    type(DynamicString_t), intent(in) :: strg
    character(len=*),      intent(in) :: sub
    logical,     intent(in), optional :: back
    integer                           :: res
    res = index(_ptr(strg), sub, back)
    _release_weak( strg )
  end function

  function cs_idx_ds( strg, sub, back ) result(res)
    character(len=*),      intent(in) :: strg
    type(DynamicString_t), intent(in) :: sub
    logical,     intent(in), optional :: back
    integer                           :: res
    res = index(strg, _ptr(sub), back)
    _release_weak( sub )
  end function

  function ds_idx_ds( strg, sub, back ) result(res)
    type(DynamicString_t), intent(in) :: strg
    type(DynamicString_t), intent(in) :: sub
    logical,     intent(in), optional :: back
    integer                           :: res
    res = index(_ptr(strg), _ptr(sub), back)
    _release_weak( strg )
    _release_weak( sub )
  end function


  ! len
  function ds_len( ds ) result(length)
    type(DynamicString_t), intent(in) :: ds
    integer                           :: length
    length = _len(ds)
    _release_weak( ds )
  end function


  ! len_trim
  function ds_len_trim( ds ) result(length)
    type(DynamicString_t), intent(in) :: ds
    integer                           :: length
    length = len_trim(_ptr(ds))
    _release_weak( ds )
  end function


  ! lge
  function ds_lge_cs( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    character(len=*),      intent(in) :: strgB
    logical                           :: res
    res = lge(_ptr(strgA), strgB)
    _release_weak( strgA )
  end function

  function cs_lge_ds( strgA, strgB ) result(res)
    character(len=*),      intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = lge(strgA, _ptr(strgB))
    _release_weak( strgB )
  end function

  function ds_lge_ds( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = lge(_ptr(strgA), _ptr(strgB))
    _release_weak( strgA )
    _release_weak( strgB )
  end function


  ! lgt
  function ds_lgt_cs( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    character(len=*),      intent(in) :: strgB
    logical                           :: res
    res = lgt(_ptr(strgA), strgB)
    _release_weak( strgA )
  end function

  function cs_lgt_ds( strgA, strgB ) result(res)
    character(len=*),      intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = lgt(strgA, _ptr(strgB))
    _release_weak( strgB )
  end function

  function ds_lgt_ds( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = lgt(_ptr(strgA), _ptr(strgB))
    _release_weak( strgA )
    _release_weak( strgB )
  end function


  ! lle
  function ds_lle_cs( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    character(len=*),      intent(in) :: strgB
    logical                           :: res
    res = lle(_ptr(strgA), strgB)
    _release_weak( strgA )
  end function

  function cs_lle_ds( strgA, strgB ) result(res)
    character(len=*),      intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = lle(strgA, _ptr(strgB))
    _release_weak( strgB )
  end function

  function ds_lle_ds( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = lle(_ptr(strgA), _ptr(strgB))
    _release_weak( strgA )
    _release_weak( strgB )
  end function


  ! llt
  function ds_llt_cs( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    character(len=*),      intent(in) :: strgB
    logical                           :: res
    res = llt(_ptr(strgA), strgB)
    _release_weak( strgA )
  end function

  function cs_llt_ds( strgA, strgB ) result(res)
    character(len=*),      intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = llt(strgA, _ptr(strgB))
    _release_weak( strgB )
  end function

  function ds_llt_ds( strgA, strgB ) result(res)
    type(DynamicString_t), intent(in) :: strgA
    type(DynamicString_t), intent(in) :: strgB
    logical                           :: res
    res = llt(_ptr(strgA), _ptr(strgB))
    _release_weak( strgA )
    _release_weak( strgB )
  end function


  ! concatenation

  function ds_concat_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    character(len=_len(lhs)+len(rhs)) :: res
    res = _ptr(lhs) // rhs
    _release_weak( lhs )
  end function

  function cs_concat_ds( lhs, rhs ) result(res)
    character(len=*),       intent(in) :: lhs
    type(DynamicString_t),  intent(in) :: rhs
    character(len=len(lhs)+_len(rhs))  :: res
    res = lhs // _ptr(rhs)
    _release_weak( rhs )
  end function

  function ds_concat_ds( lhs, rhs ) result(res)
    type(DynamicString_t),  intent(in) :: lhs
    type(DynamicString_t),  intent(in) :: rhs
    character(len=_len(lhs)+_len(rhs)) :: res
    res = _ptr(lhs) // _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function

  ! equality

  function ds_eq_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    logical                           :: res
    res = _ptr(lhs) == rhs
    _release_weak( lhs )
  end function

  function cs_eq_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = lhs == _ptr(rhs)
    _release_weak( rhs )
  end function

  function ds_eq_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = _ptr(lhs) == _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function

  ! inequality

  function ds_ne_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    logical                           :: res
    res = _ptr(lhs) /= rhs
    _release_weak( lhs )
  end function

  function cs_ne_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = lhs /= _ptr(rhs)
    _release_weak( rhs )
  end function

  function ds_ne_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = _ptr(lhs) /= _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function

  ! lower than

  function ds_lt_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    logical                           :: res
    res = _ptr(lhs) < rhs
    _release_weak( lhs )
  end function

  function cs_lt_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = lhs < _ptr(rhs)
    _release_weak( rhs )
  end function

  function ds_lt_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = _ptr(lhs) < _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function

  ! lower equal

  function ds_le_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    logical                           :: res
    res = _ptr(lhs) <= rhs
    _release_weak( lhs )
  end function

  function cs_le_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = lhs <= _ptr(rhs)
    _release_weak( rhs )
  end function

  function ds_le_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = _ptr(lhs) <= _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function

  ! greater than

  function ds_gt_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    logical                           :: res
    res = _ptr(lhs) > rhs
    _release_weak( lhs )
  end function

  function cs_gt_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = lhs > _ptr(rhs)
    _release_weak( rhs )
  end function


  function ds_gt_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = _ptr(lhs) > _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function

  ! greater equal

  function ds_ge_cs( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    logical                           :: res
    res = _ptr(lhs) >= rhs
    _release_weak( lhs )
  end function

  function cs_ge_ds( lhs, rhs ) result(res)
    character(len=*),      intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = lhs >= _ptr(rhs)
    _release_weak( rhs )
  end function

  function ds_ge_ds( lhs, rhs ) result(res)
    type(DynamicString_t), intent(in) :: lhs
    type(DynamicString_t), intent(in) :: rhs
    logical                           :: res
    res = _ptr(lhs) >= _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function

end module

