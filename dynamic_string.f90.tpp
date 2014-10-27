
module dynamic_string
  use iso_c_binding
  use generic_ref
  use adt_basestring, only: BaseString_t, &
                            attribute_permanent, attribute_volatile, permanent_string, temporary_string, &
                            basestring_len_ref, basestring_ptr, basestring_release_weak
  implicit none
  private


  type, public :: DynamicString_t
    private
    type(BaseString_t) :: str

#   define _refstat(self)   self%str%refstat
#   define _len(self)       self%str%len
#   define _ptr(self)       basestring_ptr(self%str)
#   define _array(self)     self%str%ptr(:self%str%len)
#   define _reflen(self)    basestring_len_ref( self%str )
  end type


  !_TypeGen_declare_RefType( public, DynamicString, type(DynamicString_t), scalar, \
  !     initProc   = basestring_init_by_proto, \
  !     assignProc = basestring_assign_bs,  \
  !     deleteProc = basestring_delete, \
  !     cloneMode  = _type )


  ! interface definitions

  interface DynamicString; module procedure ds_from_cs, ds_from_buf         ; end interface
  interface str          ; module procedure ds_str                          ; end interface
  interface char         ; module procedure ds_char, ds_char_l              ; end interface
  interface achar        ; module procedure ds_achar, ds_achar_l            ; end interface
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


  ! redefine adt_basestring interfaces for reusage
  
  interface
    subroutine basestring_init_by_proto( ds, has_proto, proto )
      import DynamicString_t
      type(DynamicString_t), intent(inout) :: ds
      integer,               intent(in)    :: has_proto
      type(DynamicString_t), intent(in)    :: proto
    end subroutine
  end interface

  interface cptr
    function basestring_cptr( ds ) result(res)
      import DynamicString_t, c_ptr
      type(DynamicString_t), intent(in) :: ds
      type(c_ptr)                       :: res
    end function
  end interface

  interface set_attribute
    subroutine basestring_set_attribute( ds, attrib )
      import DynamicString_t
      type(DynamicString_t),  intent(inout) :: ds
      integer(kind=1),        intent(in)    :: attrib
    end subroutine
  end interface

  interface delete
    subroutine basestring_delete( ds )
      import DynamicString_t
      type(DynamicString_t), intent(inout) :: ds
    end subroutine
  end interface
  ! assignment and operators

  interface assign
    module procedure cs_assign_ds, ds_assign_cs, ds_assign_ds, ds_assign_buf
  end interface

  interface assignment(=)
    module procedure cs_assign_ds

    subroutine basestring_assign_bs( lhs, rhs )
      import DynamicString_t
      type(DynamicString_t), intent(inout) :: lhs
      type(DynamicString_t),    intent(in) :: rhs
    end subroutine

    subroutine basestring_assign_cs( bs, cs )
      import DynamicString_t
      type(DynamicString_t), intent(inout) :: bs
      character(len=*),   intent(in)    :: cs
    end subroutine

    subroutine basestring_assign_buf( lhs, rhs )
      import DynamicString_t
      type(DynamicString_t),             intent(inout) :: lhs
      character(len=1), dimension(:), intent(in)    :: rhs
    end subroutine
  end interface

  interface operator(//)  ; module procedure ds_concat_cs, cs_concat_ds, ds_concat_ds                ; end interface
  interface operator(==)  ; module procedure ds_eq_cs, cs_eq_ds, ds_eq_ds                            ; end interface 
  interface operator(/=)  ; module procedure ds_ne_cs, cs_ne_ds, ds_ne_ds                            ; end interface 
  interface operator(<)   ; module procedure ds_lt_cs, cs_lt_ds, ds_lt_ds                            ; end interface 
  interface operator(<=)  ; module procedure ds_le_cs, cs_le_ds, ds_le_ds                            ; end interface 
  interface operator(>)   ; module procedure ds_gt_cs, cs_gt_ds, ds_gt_ds                            ; end interface 
  interface operator(>=)  ; module procedure ds_ge_cs, cs_ge_ds, ds_ge_ds                            ; end interface

  ! declare public interfaces 

  !public :: DynamicString <= is set public by RefType declaration!
  public :: str, cptr
  public :: char
  public :: delete
  public :: attribute_permanent, attribute_volatile, set_attribute
  public :: permanent_string, temporary_string

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
  public :: assign, assignment(=)
  public :: operator(//)
  public :: operator(==)
  public :: operator(/=)
  public :: operator(<)
  public :: operator(<=)
  public :: operator(>)
  public :: operator(>=)
  

# define _release_weak( ds ) \
    call basestring_release_weak( ds%str )


!-----------------
  contains
!-----------------

  !_TypeGen_implementAll()

  ! DynamicString
  function ds_from_cs( cs ) result(ds)
    use adt_basestring, only: basestring_init_by_cs
    character(len=*),    intent(in) :: cs
    type(DynamicString_t)           :: ds
    call basestring_init_by_cs( ds%str, cs )
  end function


  function ds_from_buf( buf ) result(ds)
    use adt_basestring, only: basestring_init_by_buf
    character(len=1), dimension(:), intent(in) :: buf
    type(DynamicString_t)                      :: ds
    call basestring_init_by_buf( ds%str, buf )
  end function


  ! str
  function ds_str( ds ) result(res)
    type(DynamicString_t)               :: ds
    character(len=_reflen(ds)), pointer :: res
    type(c_ptr)                         :: ptr
    ptr = cptr( ds )
    if (c_associated( ptr )) then; call c_f_pointer( ptr, res )
                             else; res => null()
    end if
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


  ! assignments

  subroutine ds_assign_cs( lhs, rhs )
    type(DynamicString_t), intent(inout) :: lhs
    character(len=*),         intent(in) :: rhs
    call basestring_assign_cs( lhs, rhs )
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
    call basestring_assign_bs( lhs, rhs )
  end subroutine


  subroutine ds_assign_buf( lhs, rhs )
    type(DynamicString_t),       intent(inout) :: lhs
    character(len=1), dimension(:), intent(in) :: rhs
    call basestring_assign_buf( lhs, rhs )
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

