
#include "adt/itfUtil.fpp"

module adt_string__
  use adt_string
  use adt_basestring
  use iso_c_binding

# define String_t   String_t__impl__

  type, public :: String_t
    type(BaseString_t) :: str

#   define _len(self)           self%str%len
#   define _ptr(self)           basestring_ptr(self%str)
#   define _array(self)         self%str%ptr(:self%str%len)
#   define _reflen(self)        basestring_len_ref( self%str )
#   define _release_weak(self)  call basestring_release_weak( self%str )
  end type

end module

!_PROC_EXPORT(string_object_size)
  integer(kind=4) &
  function string_object_size() result(res)
    use adt_string__; implicit none
    type (String_t) :: tmp
    res = storage_size(tmp) / 8
  end function


  ! String

!_PROC_EXPORT(string_from_charString)
  function string_from_charString( cs ) result(res)
    use adt_string__; implicit none
    character(len=*), intent(in) :: cs
    type(String_t)               :: res
    call basestring_init_by_charString( res%str, attribute_volatile, cs )
  end function

!_PROC_EXPORT(string_from_buf)
  function string_from_buf( buf ) result(res)
    use adt_string__; implicit none
    character(len=1), dimension(:), intent(in) :: buf
    type(String_t)                             :: res
    call basestring_init_by_buf( res%str, attribute_volatile, buf )
  end function


  ! str
!_PROC_EXPORT(string_str)
  function string_str( self ) result(res)
    use adt_string__; implicit none
    type(String_t)                        :: self
    character(len=_reflen(self)), pointer :: res
    type(c_ptr)                           :: ptr
    ptr = basestring_cptr( self%str )
    if (c_associated( ptr )) then; call c_f_pointer( ptr, res )
                             else; res => null()
    end if
  end function


  ! char
!_PROC_EXPORT(string_char)
  function string_char( self ) result(res)
    use adt_string__; implicit none
    type(String_t)            :: self
    character(len=_len(self)) :: res
    res = _ptr(self)
    _release_weak( self )
  end function


  ! char - fixed length
!_PROC_EXPORT(string_char_l)
  function string_char_l( self, length ) result(res)
    use adt_string__; implicit none
    type(String_t)        :: self
    integer,   intent(in) :: length
    character(len=length) :: res
    integer               :: limit

    limit = min(_len(self), length)
    res(1:limit)  = _ptr(self)
    res(limit+1:) = ' '
    _release_weak( self )
  end function
  

  ! achar
!_PROC_EXPORT(string_achar)
  function string_achar( self ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: self
    character(len=1)           :: res(_len(self))
    res = _array(self)
    _release_weak( self )
  end function


  ! achar - fixed length
!_PROC_EXPORT(string_achar_l)
  function string_achar_l( self, length ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: self
    integer,   intent(in)      :: length
    character(len=1)           :: res(length)
    integer                    :: limit

    limit = min(_len(self), length)
    res(1:limit)  = _array(self)
    res(limit+1:) = ' '
    _release_weak( self )
  end function


  ! assignments
!_PROC_EXPORT(charString_assign_string)
  subroutine charString_assign_string( lhs, rhs )
    use adt_string__; implicit none
    character(len=*), intent(out) :: lhs
    type(String_t),    intent(in) :: rhs
    lhs = _ptr(rhs)
    _release_weak( rhs )
  end subroutine

!_PROC_EXPORT(string_assign_charString)
  subroutine string_assign_charString( lhs, rhs )
    use adt_string__; implicit none
    type(String_t), intent(inout) :: lhs
    character(len=*),  intent(in) :: rhs
    call basestring_assign_charString( lhs%str, rhs )
  end subroutine

!_PROC_EXPORT(string_assign_string)
  subroutine string_assign_string( lhs, rhs )
    use adt_string__; implicit none
    type(String_t), intent(inout) :: lhs
    type(String_t),    intent(in) :: rhs
    call basestring_assign_basestring( lhs%str, rhs%str )
  end subroutine

!_PROC_EXPORT(string_assign_buf)
  subroutine string_assign_buf( lhs, rhs )
    use adt_string__; implicit none
    type(String_t),              intent(inout) :: lhs
    character(len=1), dimension(:), intent(in) :: rhs
    call basestring_assign_buf( lhs%str, rhs )
  end subroutine


!##################################

  ! adjustl
!_PROC_EXPORT(string_adjustl)
  function string_adjustl( self ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: self
    character(len=_len(self))  :: res
    res = adjustl(_ptr(self))
    _release_weak( self )
  end function

  ! adjustr
!_PROC_EXPORT(string_adjustr)
  function string_adjustr( self ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: self
    character(len=_len(self))  :: res
    res = adjustr(_ptr(self))
    _release_weak( self )
  end function


  ! iachar
!_PROC_EXPORT(string_iachar)
  function string_iachar( self ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: self
    integer                    :: res(_len(self))
    res = iachar(_array(self))
    _release_weak( self )
  end function

  
  ! ichar
!_PROC_EXPORT(string_ichar)
  function string_ichar( self ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: self
    integer                    :: res(_len(self))
    res = ichar(_array(self))
    _release_weak( self )
  end function


  ! index
!_PROC_EXPORT(string_idx_charString)
  function string_idx_charString( strg, sub, back ) result(res)
    use adt_string__; implicit none
    type(String_t),        intent(in) :: strg
    character(len=*),      intent(in) :: sub
    logical,     intent(in), optional :: back
    integer                           :: res
    res = index(_ptr(strg), sub, back)
    _release_weak( strg )
  end function

!_PROC_EXPORT(charString_idx_string)
  function charString_idx_string( strg, sub, back ) result(res)
    use adt_string__; implicit none
    character(len=*),  intent(in) :: strg
    type(String_t),    intent(in) :: sub
    logical, optional, intent(in) :: back
    integer                       :: res
    res = index(strg, _ptr(sub), back)
    _release_weak( sub )
  end function

!_PROC_EXPORT(string_idx_string)
  function string_idx_string( strg, sub, back ) result(res)
    use adt_string__; implicit none
    type(String_t),    intent(in) :: strg
    type(String_t),    intent(in) :: sub
    logical, optional, intent(in) :: back
    integer                       :: res
    res = index(_ptr(strg), _ptr(sub), back)
    _release_weak( strg )
    _release_weak( sub )
  end function


  ! len
!_PROC_EXPORT(string_len)
  function string_len( self ) result(length)
    use adt_string__; implicit none
    type(String_t), intent(in) :: self
    integer                    :: length
    length = _len(self)
    _release_weak( self )
  end function


  ! len_trim
!_PROC_EXPORT(string_len_trim)
  function string_len_trim( self ) result(length)
    use adt_string__; implicit none
    type(String_t), intent(in) :: self
    integer                    :: length
    length = len_trim(_ptr(self))
    _release_weak( self )
  end function


  ! lge
!_PROC_EXPORT(string_lge_charString)
  function string_lge_charString( strgA, strgB ) result(res)
    use adt_string__; implicit none
    type(String_t),   intent(in) :: strgA
    character(len=*), intent(in) :: strgB
    logical                      :: res
    res = lge(_ptr(strgA), strgB)
    _release_weak( strgA )
  end function

!_PROC_EXPORT(charString_lge_string)
  function charString_lge_string( strgA, strgB ) result(res)
    use adt_string__; implicit none
    character(len=*), intent(in) :: strgA
    type(String_t),   intent(in) :: strgB
    logical                      :: res
    res = lge(strgA, _ptr(strgB))
    _release_weak( strgB )
  end function

!_PROC_EXPORT(string_lge_string)
  function string_lge_string( strgA, strgB ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: strgA
    type(String_t), intent(in) :: strgB
    logical                    :: res
    res = lge(_ptr(strgA), _ptr(strgB))
    _release_weak( strgA )
    _release_weak( strgB )
  end function


  ! lgt
!_PROC_EXPORT(string_lgt_charString)
  function string_lgt_charString( strgA, strgB ) result(res)
    use adt_string__; implicit none
    type(String_t),   intent(in) :: strgA
    character(len=*), intent(in) :: strgB
    logical                      :: res
    res = lgt(_ptr(strgA), strgB)
    _release_weak( strgA )
  end function

!_PROC_EXPORT(charString_lgt_string)
  function charString_lgt_string( strgA, strgB ) result(res)
    use adt_string__; implicit none
    character(len=*), intent(in) :: strgA
    type(String_t),   intent(in) :: strgB
    logical                      :: res
    res = lgt(strgA, _ptr(strgB))
    _release_weak( strgB )
  end function

!_PROC_EXPORT(string_lgt_string)
  function string_lgt_string( strgA, strgB ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: strgA
    type(String_t), intent(in) :: strgB
    logical                    :: res
    res = lgt(_ptr(strgA), _ptr(strgB))
    _release_weak( strgA )
    _release_weak( strgB )
  end function


  ! lle
!_PROC_EXPORT(string_lle_charString)
  function string_lle_charString( strgA, strgB ) result(res)
    use adt_string__; implicit none
    type(String_t),   intent(in) :: strgA
    character(len=*), intent(in) :: strgB
    logical                      :: res
    res = lle(_ptr(strgA), strgB)
    _release_weak( strgA )
  end function

!_PROC_EXPORT(charString_lle_string)
  function charString_lle_string( strgA, strgB ) result(res)
    use adt_string__; implicit none
    character(len=*), intent(in) :: strgA
    type(String_t),   intent(in) :: strgB
    logical                      :: res
    res = lle(strgA, _ptr(strgB))
    _release_weak( strgB )
  end function

!_PROC_EXPORT(string_lle_string)
  function string_lle_string( strgA, strgB ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: strgA
    type(String_t), intent(in) :: strgB
    logical                    :: res
    res = lle(_ptr(strgA), _ptr(strgB))
    _release_weak( strgA )
    _release_weak( strgB )
  end function


  ! llt
!_PROC_EXPORT(string_llt_charString)
  function string_llt_charString( strgA, strgB ) result(res)
    use adt_string__; implicit none
    type(String_t),   intent(in) :: strgA
    character(len=*), intent(in) :: strgB
    logical                      :: res
    res = llt(_ptr(strgA), strgB)
    _release_weak( strgA )
  end function

!_PROC_EXPORT(charString_llt_string)
  function charString_llt_string( strgA, strgB ) result(res)
    use adt_string__; implicit none
    character(len=*), intent(in) :: strgA
    type(String_t),   intent(in) :: strgB
    logical                      :: res
    res = llt(strgA, _ptr(strgB))
    _release_weak( strgB )
  end function

!_PROC_EXPORT(string_llt_string)
  function string_llt_string( strgA, strgB ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: strgA
    type(String_t), intent(in) :: strgB
    logical                    :: res
    res = llt(_ptr(strgA), _ptr(strgB))
    _release_weak( strgA )
    _release_weak( strgB )
  end function


  ! concatenation
!_PROC_EXPORT(string_concat_charString)
  function string_concat_charString( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t),        intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    character(len=_len(lhs)+len(rhs)) :: res
    res = _ptr(lhs) // rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charString_concat_string)
  function charString_concat_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    character(len=*),      intent(in) :: lhs
    type(String_t),        intent(in) :: rhs
    character(len=len(lhs)+_len(rhs)) :: res
    res = lhs // _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_concat_string)
  function string_concat_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t),         intent(in) :: lhs
    type(String_t),         intent(in) :: rhs
    character(len=_len(lhs)+_len(rhs)) :: res
    res = _ptr(lhs) // _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


  ! equality
!_PROC_EXPORT(string_eq_charString)
  function string_eq_charString( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t),   intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: res
    res = _ptr(lhs) == rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charString_eq_string)
  function charString_eq_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    character(len=*), intent(in) :: lhs
    type(String_t),   intent(in) :: rhs
    logical                      :: res
    res = lhs == _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_eq_string)
  function string_eq_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: lhs
    type(String_t), intent(in) :: rhs
    logical                    :: res
    res = _ptr(lhs) == _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


  ! inequality
!_PROC_EXPORT(string_ne_charString)
  function string_ne_charString( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t),   intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: res
    res = _ptr(lhs) /= rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charString_ne_string)
  function charString_ne_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    character(len=*), intent(in) :: lhs
    type(String_t),   intent(in) :: rhs
    logical                      :: res
    res = lhs /= _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_ne_string)
  function string_ne_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: lhs
    type(String_t), intent(in) :: rhs
    logical                    :: res
    res = _ptr(lhs) /= _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


  ! lower than
!_PROC_EXPORT(string_lt_charString)
  function string_lt_charString( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t),   intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: res
    res = _ptr(lhs) < rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charString_lt_string)
  function charString_lt_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    character(len=*), intent(in) :: lhs
    type(String_t),   intent(in) :: rhs
    logical                      :: res
    res = lhs < _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_lt_string)
  function string_lt_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: lhs
    type(String_t), intent(in) :: rhs
    logical                    :: res
    res = _ptr(lhs) < _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


  ! lower equal
!_PROC_EXPORT(string_le_charString)
  function string_le_charString( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t),   intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: res
    res = _ptr(lhs) <= rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charString_le_string)
  function charString_le_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    character(len=*), intent(in) :: lhs
    type(String_t),   intent(in) :: rhs
    logical                      :: res
    res = lhs <= _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_le_string)
  function string_le_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: lhs
    type(String_t), intent(in) :: rhs
    logical                    :: res
    res = _ptr(lhs) <= _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


  ! greater than
!_PROC_EXPORT(string_gt_charString)
  function string_gt_charString( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t),   intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: res
    res = _ptr(lhs) > rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charString_gt_string)
  function charString_gt_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    character(len=*), intent(in) :: lhs
    type(String_t),   intent(in) :: rhs
    logical                      :: res
    res = lhs > _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_gt_string)
  function string_gt_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: lhs
    type(String_t), intent(in) :: rhs
    logical                    :: res
    res = _ptr(lhs) > _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


  ! greater equal
!_PROC_EXPORT(string_ge_charString)
  function string_ge_charString( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t),   intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: res
    res = _ptr(lhs) >= rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charString_ge_string)
  function charString_ge_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    character(len=*), intent(in) :: lhs
    type(String_t),   intent(in) :: rhs
    logical                      :: res
    res = lhs >= _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_ge_string)
  function string_ge_string( lhs, rhs ) result(res)
    use adt_string__; implicit none
    type(String_t), intent(in) :: lhs
    type(String_t), intent(in) :: rhs
    logical                    :: res
    res = _ptr(lhs) >= _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function
