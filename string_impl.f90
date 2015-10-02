
#include "adt/itfUtil.fpp"

module impl_string__
  use adt_string
  use adt_basestring
  use iso_c_binding

# define String_t   String_t__impl__

  type, public :: String_t
    type(BaseString_t) :: str

#   define _len(self)           self%str%len
#   define _ptr(self)           basestring_ptr(self%str)
#   define _array(self)         self%str%ptr(:self%str%len)
#   define _charAt(self,idx)    self%str%ptr(idx)
#   define _reflen(self)        basestring_len_ref( self%str )
#   define _release_weak(self)  call basestring_release_weak( self%str )
  end type

  integer, parameter :: uc_A = iachar('A')
  integer, parameter :: uc_Z = iachar('Z')
  integer, parameter :: lc_a = iachar('a')
  integer, parameter :: lc_z = iachar('z')

  interface
    subroutine charstring_to_upper( cs )
      character(len=*) :: cs
    end subroutine

    subroutine charstring_to_lower( cs )
      character(len=*) :: cs
    end subroutine

    function string_upper( ds ) result(res)
      import String_t
      type(String_t)          :: ds
      character(len=_len(ds)) :: res
    end function

    function string_lower( ds ) result(res)
      import String_t
      type(String_t)          :: ds
      character(len=_len(ds)) :: res
    end function
  end interface

end module

!_PROC_EXPORT(string_object_size_c)
  integer(kind=4) &
  function string_object_size_c() result(res)
    use impl_string__; implicit none
    type (String_t) :: tmp
    res = storage_size(tmp) / 8
  end function


  ! String

!_PROC_EXPORT(string_from_charstring)
  function string_from_charstring( cs ) result(res)
    use impl_string__; implicit none
    character(len=*), intent(in) :: cs
    type(String_t)               :: res
    call basestring_init_by_charstring_c( res%str, attribute_volatile, cs )
  end function

!_PROC_EXPORT(string_from_buf)
  function string_from_buf( buf ) result(res)
    use impl_string__; implicit none
    character(len=1), dimension(:), intent(in) :: buf
    type(String_t)                             :: res
    call basestring_init_by_buf( res%str, attribute_volatile, buf )
  end function


  ! str
!_PROC_EXPORT(string_str)
  function string_str( self ) result(res)
    use impl_string__; implicit none
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
    use impl_string__; implicit none
    type(String_t)            :: self
    character(len=_len(self)) :: res
    res = _ptr(self)
    _release_weak( self )
  end function


  ! char - fixed length
!_PROC_EXPORT(string_char_l)
  function string_char_l( self, length ) result(res)
    use impl_string__; implicit none
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
    use impl_string__; implicit none
    type(String_t), intent(in) :: self
    character(len=1)           :: res(_len(self))
    res = _array(self)
    _release_weak( self )
  end function


  ! achar - fixed length
!_PROC_EXPORT(string_achar_l)
  function string_achar_l( self, length ) result(res)
    use impl_string__; implicit none
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
!_PROC_EXPORT(charstring_assign_string_c)
  subroutine charstring_assign_string_c( lhs, rhs )
    use impl_string__; implicit none
    character(len=*), intent(out) :: lhs
    type(String_t),    intent(in) :: rhs
    lhs = _ptr(rhs)
    _release_weak( rhs )
  end subroutine

!##################################

!_PROC_EXPORT(charstring_to_lower)
  subroutine charstring_to_lower( cs )
    use impl_string__, only: uc_A, uc_Z
    implicit none
    character(len=*)   :: cs
    integer            :: i, ac

    do i = 1, len(cs)
      ac = iachar(cs(i:i))
      if (ac >= uc_A .and. ac <= uc_Z) &
        cs(i:i) = achar( ibset( ac, 5 ) )
    end do
  end subroutine

!_PROC_EXPORT(charstring_to_upper)
  subroutine charstring_to_upper( cs )
    use impl_string__, only: lc_a, lc_z
    implicit none
    character(len=*)   :: cs
    integer            :: i, ac

    do i = 1, len(cs)
      ac = iachar(cs(i:i))
      if (ac >= lc_a .and. ac <= lc_z) &
        cs(i:i) = achar( ibclr( ac, 5 ) )
    end do
  end subroutine


!_PROC_EXPORT(charstring_lower)
  function charstring_lower( cs ) result(res)
    use impl_string__; implicit none
    character(len=*)       :: cs
    character(len=len(cs)) :: res
    res = cs
    call charstring_to_lower( res )
  end function

!_PROC_EXPORT(charstring_upper)
  function charstring_upper( cs ) result(res)
    use impl_string__; implicit none
    character(len=*)       :: cs
    character(len=len(cs)) :: res
    res = cs
    call charstring_to_upper( res )
  end function


!_PROC_EXPORT(string_lower)
  function string_lower( self ) result(res)
    use impl_string__, only: String_t, charstring_to_lower, basestring_ptr
    implicit none
    type(String_t)            :: self
    character(len=_len(self)) :: res
    res = _ptr(self)
    call charstring_to_lower( res )
    _release_weak( self )
  end function

!_PROC_EXPORT(string_upper)
  function string_upper( self ) result(res)
    use impl_string__, only: String_t, charstring_to_upper, basestring_ptr
    implicit none
    type(String_t)            :: self
    character(len=_len(self)) :: res
    res = _ptr(self)
    call charstring_to_upper( res )
    _release_weak( self )
  end function


!_PROC_EXPORT(string_to_lower)
  subroutine string_to_lower( self )
    use impl_string__; implicit none
    type(String_t) :: self
    call basestring_to_lower( self%str )
    _release_weak( self )
  end subroutine

!_PROC_EXPORT(string_to_upper)
  subroutine string_to_upper( self )
    use impl_string__; implicit none
    type(String_t) :: self
    call basestring_to_upper( self%str )
    _release_weak( self )
  end subroutine


  ! adjustl
!_PROC_EXPORT(string_adjustl)
  function string_adjustl( self ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: self
    character(len=_len(self))  :: res
    res = adjustl(_ptr(self))
    _release_weak( self )
  end function

  ! adjustr
!_PROC_EXPORT(string_adjustr)
  function string_adjustr( self ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: self
    character(len=_len(self))  :: res
    res = adjustr(_ptr(self))
    _release_weak( self )
  end function


  ! iachar
!_PROC_EXPORT(string_iachar)
  function string_iachar( self ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: self
    integer                    :: res(_len(self))
    res = iachar(_array(self))
    _release_weak( self )
  end function

  
  ! ichar
!_PROC_EXPORT(string_ichar)
  function string_ichar( self ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: self
    integer                    :: res(_len(self))
    res = ichar(_array(self))
    _release_weak( self )
  end function


  ! index
!_PROC_EXPORT(string_idx_charstring)
  function string_idx_charstring( strg, sub, back ) result(res)
    use impl_string__; implicit none
    type(String_t),        intent(in) :: strg
    character(len=*),      intent(in) :: sub
    logical,     intent(in), optional :: back
    integer                           :: res
    res = index(_ptr(strg), sub, back)
    _release_weak( strg )
  end function

!_PROC_EXPORT(charstring_idx_string)
  function charstring_idx_string( strg, sub, back ) result(res)
    use impl_string__; implicit none
    character(len=*),  intent(in) :: strg
    type(String_t),    intent(in) :: sub
    logical, optional, intent(in) :: back
    integer                       :: res
    res = index(strg, _ptr(sub), back)
    _release_weak( sub )
  end function

!_PROC_EXPORT(string_idx_string)
  function string_idx_string( strg, sub, back ) result(res)
    use impl_string__; implicit none
    type(String_t),    intent(in) :: strg
    type(String_t),    intent(in) :: sub
    logical, optional, intent(in) :: back
    integer                       :: res
    res = index(_ptr(strg), _ptr(sub), back)
    _release_weak( strg )
    _release_weak( sub )
  end function


  ! len
!_PROC_EXPORT(string_len_c)
  function string_len_c( self ) result(length)
    use impl_string__; implicit none
    type(String_t), intent(in) :: self
    integer(kind=c_size_t)     :: length
    length = _len(self)
    _release_weak( self )
  end function


  ! len_trim
!_PROC_EXPORT(string_len_trim_c)
  function string_len_trim_c( self ) result(length)
    use impl_string__; implicit none
    type(String_t), intent(in) :: self
    integer(kind=c_size_t)     :: length
    length = len_trim(_ptr(self))
    _release_weak( self )
  end function


  ! lge
!_PROC_EXPORT(string_lge_charstring_c)
  function string_lge_charstring_c( strgA, strgB ) result(res)
    use impl_string__; implicit none
    type(String_t),   intent(in) :: strgA
    character(len=*), intent(in) :: strgB
    logical                      :: res
    res = lge(_ptr(strgA), strgB)
    _release_weak( strgA )
  end function

!_PROC_EXPORT(charstring_lge_string_c)
  function charstring_lge_string_c( strgA, strgB ) result(res)
    use impl_string__; implicit none
    character(len=*), intent(in) :: strgA
    type(String_t),   intent(in) :: strgB
    logical                      :: res
    res = lge(strgA, _ptr(strgB))
    _release_weak( strgB )
  end function

!_PROC_EXPORT(string_lge_string_c)
  function string_lge_string_c( strgA, strgB ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: strgA
    type(String_t), intent(in) :: strgB
    logical                    :: res
    res = lge(_ptr(strgA), _ptr(strgB))
    _release_weak( strgA )
    _release_weak( strgB )
  end function


  ! lgt
!_PROC_EXPORT(string_lgt_charstring_c)
  function string_lgt_charstring_c( strgA, strgB ) result(res)
    use impl_string__; implicit none
    type(String_t),   intent(in) :: strgA
    character(len=*), intent(in) :: strgB
    logical                      :: res
    res = lgt(_ptr(strgA), strgB)
    _release_weak( strgA )
  end function

!_PROC_EXPORT(charstring_lgt_string_c)
  function charstring_lgt_string_c( strgA, strgB ) result(res)
    use impl_string__; implicit none
    character(len=*), intent(in) :: strgA
    type(String_t),   intent(in) :: strgB
    logical                      :: res
    res = lgt(strgA, _ptr(strgB))
    _release_weak( strgB )
  end function

!_PROC_EXPORT(string_lgt_string_c)
  function string_lgt_string_c( strgA, strgB ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: strgA
    type(String_t), intent(in) :: strgB
    logical                    :: res
    res = lgt(_ptr(strgA), _ptr(strgB))
    _release_weak( strgA )
    _release_weak( strgB )
  end function


  ! lle
!_PROC_EXPORT(string_lle_charstring_c)
  function string_lle_charstring_c( strgA, strgB ) result(res)
    use impl_string__; implicit none
    type(String_t),   intent(in) :: strgA
    character(len=*), intent(in) :: strgB
    logical                      :: res
    res = lle(_ptr(strgA), strgB)
    _release_weak( strgA )
  end function

!_PROC_EXPORT(charstring_lle_string_c)
  function charstring_lle_string_c( strgA, strgB ) result(res)
    use impl_string__; implicit none
    character(len=*), intent(in) :: strgA
    type(String_t),   intent(in) :: strgB
    logical                      :: res
    res = lle(strgA, _ptr(strgB))
    _release_weak( strgB )
  end function

!_PROC_EXPORT(string_lle_string_c)
  function string_lle_string_c( strgA, strgB ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: strgA
    type(String_t), intent(in) :: strgB
    logical                    :: res
    res = lle(_ptr(strgA), _ptr(strgB))
    _release_weak( strgA )
    _release_weak( strgB )
  end function


  ! llt
!_PROC_EXPORT(string_llt_charstring_c)
  function string_llt_charstring_c( strgA, strgB ) result(res)
    use impl_string__; implicit none
    type(String_t),   intent(in) :: strgA
    character(len=*), intent(in) :: strgB
    logical                      :: res
    res = llt(_ptr(strgA), strgB)
    _release_weak( strgA )
  end function

!_PROC_EXPORT(charstring_llt_string_c)
  function charstring_llt_string_c( strgA, strgB ) result(res)
    use impl_string__; implicit none
    character(len=*), intent(in) :: strgA
    type(String_t),   intent(in) :: strgB
    logical                      :: res
    res = llt(strgA, _ptr(strgB))
    _release_weak( strgB )
  end function

!_PROC_EXPORT(string_llt_string_c)
  function string_llt_string_c( strgA, strgB ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: strgA
    type(String_t), intent(in) :: strgB
    logical                    :: res
    res = llt(_ptr(strgA), _ptr(strgB))
    _release_weak( strgA )
    _release_weak( strgB )
  end function


  ! concatenation
!_PROC_EXPORT(string_concat_charstring)
  function string_concat_charstring( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t),        intent(in) :: lhs
    character(len=*),      intent(in) :: rhs
    character(len=_len(lhs)+len(rhs)) :: res
    res = _ptr(lhs) // rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charstring_concat_string)
  function charstring_concat_string( lhs, rhs ) result(res)
    use impl_string__; implicit none
    character(len=*),      intent(in) :: lhs
    type(String_t),        intent(in) :: rhs
    character(len=len(lhs)+_len(rhs)) :: res
    res = lhs // _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_concat_string)
  function string_concat_string( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t),         intent(in) :: lhs
    type(String_t),         intent(in) :: rhs
    character(len=_len(lhs)+_len(rhs)) :: res
    res = _ptr(lhs) // _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


  ! equality
!_PROC_EXPORT(string_eq_charstring_c)
  function string_eq_charstring_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t),   intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: res
    res = _ptr(lhs) == rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charstring_eq_string_c)
  function charstring_eq_string_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    character(len=*), intent(in) :: lhs
    type(String_t),   intent(in) :: rhs
    logical                      :: res
    res = lhs == _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_eq_string_c)
  function string_eq_string_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: lhs
    type(String_t), intent(in) :: rhs
    logical                    :: res
    res = _ptr(lhs) == _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


  ! inequality
!_PROC_EXPORT(string_ne_charstring_c)
  function string_ne_charstring_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t),   intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: res
    res = _ptr(lhs) /= rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charstring_ne_string_c)
  function charstring_ne_string_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    character(len=*), intent(in) :: lhs
    type(String_t),   intent(in) :: rhs
    logical                      :: res
    res = lhs /= _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_ne_string_c)
  function string_ne_string_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: lhs
    type(String_t), intent(in) :: rhs
    logical                    :: res
    res = _ptr(lhs) /= _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


  ! lower than
!_PROC_EXPORT(string_lt_charstring_c)
  function string_lt_charstring_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t),   intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: res
    res = _ptr(lhs) < rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charstring_lt_string_c)
  function charstring_lt_string_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    character(len=*), intent(in) :: lhs
    type(String_t),   intent(in) :: rhs
    logical                      :: res
    res = lhs < _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_lt_string_c)
  function string_lt_string_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: lhs
    type(String_t), intent(in) :: rhs
    logical                    :: res
    res = _ptr(lhs) < _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


  ! lower equal
!_PROC_EXPORT(string_le_charstring_c)
  function string_le_charstring_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t),   intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: res
    res = _ptr(lhs) <= rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charstring_le_string_c)
  function charstring_le_string_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    character(len=*), intent(in) :: lhs
    type(String_t),   intent(in) :: rhs
    logical                      :: res
    res = lhs <= _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_le_string_c)
  function string_le_string_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: lhs
    type(String_t), intent(in) :: rhs
    logical                    :: res
    res = _ptr(lhs) <= _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


  ! greater than
!_PROC_EXPORT(string_gt_charstring_c)
  function string_gt_charstring_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t),   intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: res
    res = _ptr(lhs) > rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charstring_gt_string_c)
  function charstring_gt_string_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    character(len=*), intent(in) :: lhs
    type(String_t),   intent(in) :: rhs
    logical                      :: res
    res = lhs > _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_gt_string_c)
  function string_gt_string_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: lhs
    type(String_t), intent(in) :: rhs
    logical                    :: res
    res = _ptr(lhs) > _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


  ! greater equal
!_PROC_EXPORT(string_ge_charstring_c)
  function string_ge_charstring_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t),   intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: res
    res = _ptr(lhs) >= rhs
    _release_weak( lhs )
  end function

!_PROC_EXPORT(charstring_ge_string_c)
  function charstring_ge_string_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    character(len=*), intent(in) :: lhs
    type(String_t),   intent(in) :: rhs
    logical                      :: res
    res = lhs >= _ptr(rhs)
    _release_weak( rhs )
  end function

!_PROC_EXPORT(string_ge_string_c)
  function string_ge_string_c( lhs, rhs ) result(res)
    use impl_string__; implicit none
    type(String_t), intent(in) :: lhs
    type(String_t), intent(in) :: rhs
    logical                    :: res
    res = _ptr(lhs) >= _ptr(rhs)
    _release_weak( lhs )
    _release_weak( rhs )
  end function


!_PROC_EXPORT(string_file_basename_charstring)
  function string_file_basename_charstring( filePath ) result(res)
    use impl_string__; implicit none
    character(len=*)             :: filePath
    character(len=len(filePath)) :: res
    integer :: i,j
    i = max( index( filePath, '/', back=.true. ), index( filePath, '\', back=.true. ) )
    j = index( filePath, '.', back=.true. ) - 1
    j = merge( j, -1, j > i )
    j = modulo( j, len(filePath) + 1 )
    res = filePath(i+1:j)
  end function

!_PROC_EXPORT(string_file_basename_string)
  function string_file_basename_string( filePath ) result(res)
    use impl_string__; implicit none
    type(String_t)                :: filePath
    character(len=_len(filePath)) :: res
    integer :: i,j
    res = file_basename( _ptr(filePath) )
    _release_weak(filePath)
  end function

