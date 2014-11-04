
module adt_string
  use iso_c_binding
  use adt_ref
  use adt_basestring, only: BaseString_t, basestring_len_ref, &
                            attribute_permanent, attribute_volatile, permanent_string, temporary_string
  implicit none
  private


  type, public :: String_t
    private
    type(BaseString_t) :: str

#   define _len(self)       self%str%len
#   define _reflen(self)    basestring_len_ref( self%str )
  end type


  !_TypeGen_declare_RefType( public, String, type(String_t), scalar, \
  !     initProc   = basestring_init_by_proto, \
  !     assignProc = basestring_assign_basestring,  \
  !     deleteProc = basestring_delete, \
  !     cloneMode  = _type )


  !public :: String <= is set public by RefType declaration!
  public :: str, cptr
  public :: char
  public :: delete
  public :: set_attribute, attribute_permanent, attribute_volatile
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


  ! interface definitions

  interface String
    function string_from_charstring( cs ) result(ds)
      import String_t
      character(len=*), intent(in) :: cs
      type(String_t)               :: ds
    end function

    function string_from_buf( buf ) result(ds)
      import String_t
      character(len=1), dimension(:), intent(in) :: buf
      type(String_t)                             :: ds
    end function
  end interface


  interface str
    function string_str( ds ) result(res)
      import String_t
      type(String_t)                      :: ds
      character(len=_reflen(ds)), pointer :: res
    end function
  end interface


  interface cptr
    function basestring_cptr( ds ) result(res)
      import String_t, c_ptr
      type(String_t), intent(in) :: ds
      type(c_ptr)                :: res
    end function
  end interface


  interface char
    function string_char( ds ) result(res)
      import String_t
      type(String_t)          :: ds
      character(len=_len(ds)) :: res
    end function

    function string_char_l( ds, length ) result(res)
      import String_t
      type(String_t)        :: ds
      integer,   intent(in) :: length
      character(len=length) :: res
    end function
  end interface


  interface delete
    subroutine basestring_delete( ds )
      import String_t
      type(String_t), intent(inout) :: ds
    end subroutine
  end interface


  interface set_attribute
    subroutine basestring_set_attribute( ds, attrib )
      import String_t
      type(String_t),  intent(inout) :: ds
      integer(kind=1), intent(in)    :: attrib
    end subroutine
  end interface


  interface achar
    function string_achar( ds ) result(res)
      import String_t
      type(String_t), intent(in) :: ds
      character(len=1)           :: res(_len(ds))
    end function

    function string_achar_l( ds, length ) result(res)
      import String_t
      type(String_t), intent(in) :: ds
      integer,   intent(in)      :: length
      character(len=1)           :: res(length)
    end function
  end interface


  interface adjustl
    function string_adjustl( ds ) result(res)
      import String_t
      type(String_t), intent(in) :: ds
      character(len=_len(ds))    :: res
    end function
  end interface


  interface adjustr
    function string_adjustr( ds ) result(res)
      import String_t
      type(String_t), intent(in) :: ds
      character(len=_len(ds))    :: res
    end function
  end interface


  interface iachar
    function string_iachar( ds ) result(res)
      import String_t
      type(String_t), intent(in) :: ds
      integer                    :: res(_len(ds))
    end function
  end interface


  interface ichar
    function string_ichar( ds ) result(res)
      import String_t
      type(String_t), intent(in) :: ds
      integer                    :: res(_len(ds))
    end function
  end interface


  interface index
    function string_idx_charstring( strg, sub, back ) result(res)
      import String_t
      type(String_t),        intent(in) :: strg
      character(len=*),      intent(in) :: sub
      logical,     intent(in), optional :: back
      integer                           :: res
    end function

    function charstring_idx_string( strg, sub, back ) result(res)
      import String_t
      character(len=*),  intent(in) :: strg
      type(String_t),    intent(in) :: sub
      logical, optional, intent(in) :: back
      integer                       :: res
    end function

    function string_idx_string( strg, sub, back ) result(res)
      import String_t
      type(String_t),    intent(in) :: strg
      type(String_t),    intent(in) :: sub
      logical, optional, intent(in) :: back
      integer                       :: res
    end function
  end interface


  interface len
    function string_len( ds ) result(length)
      import String_t
      type(String_t), intent(in) :: ds
      integer                    :: length
    end function
  end interface

  
  interface len_trim
    function string_len_trim( ds ) result(length)
      import String_t
      type(String_t), intent(in) :: ds
      integer                    :: length
    end function
  end interface


  interface lge
    logical function string_lge_charstring( strgA, strgB )
      import String_t
      type(String_t),   intent(in) :: strgA
      character(len=*), intent(in) :: strgB
    end function

    logical function charstring_lge_string( strgA, strgB )
      import String_t
      character(len=*), intent(in) :: strgA
      type(String_t),   intent(in) :: strgB
    end function

    logical function string_lge_string( strgA, strgB )
      import String_t
      type(String_t), intent(in) :: strgA
      type(String_t), intent(in) :: strgB
    end function
  end interface


  interface lgt
    logical function string_lgt_charstring( strgA, strgB )
      import String_t
      type(String_t),   intent(in) :: strgA
      character(len=*), intent(in) :: strgB
    end function

    logical function charstring_lgt_string( strgA, strgB )
      import String_t
      character(len=*), intent(in) :: strgA
      type(String_t),   intent(in) :: strgB
    end function

    logical function string_lgt_string( strgA, strgB )
      import String_t
      type(String_t), intent(in) :: strgA
      type(String_t), intent(in) :: strgB
    end function
  end interface


  interface lle
    logical function string_lle_charstring( strgA, strgB )
      import String_t
      type(String_t),   intent(in) :: strgA
      character(len=*), intent(in) :: strgB
    end function

    logical function charstring_lle_string( strgA, strgB )
      import String_t
      character(len=*), intent(in) :: strgA
      type(String_t),   intent(in) :: strgB
    end function

    logical function string_lle_string( strgA, strgB )
      import String_t
      type(String_t), intent(in) :: strgA
      type(String_t), intent(in) :: strgB
    end function
  end interface


  interface llt
    logical function string_llt_charstring( strgA, strgB )
      import String_t
      type(String_t),   intent(in) :: strgA
      character(len=*), intent(in) :: strgB
    end function

    logical function charstring_llt_string( strgA, strgB )
      import String_t
      character(len=*), intent(in) :: strgA
      type(String_t),   intent(in) :: strgB
    end function

    logical function string_llt_string( strgA, strgB )
      import String_t
      type(String_t), intent(in) :: strgA
      type(String_t), intent(in) :: strgB
    end function
  end interface


  interface assign
    subroutine charstring_assign_string( cs, ds )
      import String_t
      character(len=*), intent(out) :: cs
      type(String_t),    intent(in) :: ds
    end subroutine

    subroutine basestring_assign_basestring( lhs, rhs )
      import String_t
      type(String_t), intent(inout) :: lhs
      type(String_t),    intent(in) :: rhs
    end subroutine

    subroutine basestring_assign_charstring( bs, cs )
      import String_t
      type(String_t), intent(inout) :: bs
      character(len=*),   intent(in)    :: cs
    end subroutine

    subroutine basestring_assign_buf( lhs, rhs )
      import String_t
      type(String_t),                 intent(inout) :: lhs
      character(len=1), dimension(:), intent(in)    :: rhs
    end subroutine
  end interface


  interface assignment(=)
    module procedure charstring_assign_string_private, string_assign_string_private
    module procedure string_assign_charstring_private, string_assign_buf_private
  end interface


  interface operator(//)
    function string_concat_charstring( lhs, rhs ) result(res)
      import String_t
      type(String_t),        intent(in) :: lhs
      character(len=*),      intent(in) :: rhs
      character(len=_len(lhs)+len(rhs)) :: res
    end function

    function charstring_concat_string( lhs, rhs ) result(res)
      import String_t
      character(len=*),      intent(in) :: lhs
      type(String_t),        intent(in) :: rhs
      character(len=len(lhs)+_len(rhs)) :: res
    end function

    function string_concat_string( lhs, rhs ) result(res)
      import String_t
      type(String_t),         intent(in) :: lhs
      type(String_t),         intent(in) :: rhs
      character(len=_len(lhs)+_len(rhs)) :: res
    end function
  end interface


  interface operator(==)
    logical function string_eq_charstring( lhs, rhs )
      import String_t
      type(String_t),   intent(in) :: lhs
      character(len=*), intent(in) :: rhs
    end function

    logical function charstring_eq_string( lhs, rhs )
      import String_t
      character(len=*), intent(in) :: lhs
      type(String_t),   intent(in) :: rhs
    end function

    logical function string_eq_string( lhs, rhs )
      import String_t
      type(String_t), intent(in) :: lhs
      type(String_t), intent(in) :: rhs
    end function
  end interface


  interface operator(/=)
    logical function string_ne_charstring( lhs, rhs )
      import String_t
      type(String_t),   intent(in) :: lhs
      character(len=*), intent(in) :: rhs
    end function

    logical function charstring_ne_string( lhs, rhs )
      import String_t
      character(len=*), intent(in) :: lhs
      type(String_t),   intent(in) :: rhs
    end function

    logical function string_ne_string( lhs, rhs )
      import String_t
      type(String_t), intent(in) :: lhs
      type(String_t), intent(in) :: rhs
    end function
  end interface


  interface operator(<) 
    logical function string_lt_charstring( lhs, rhs )
      import String_t
      type(String_t),   intent(in) :: lhs
      character(len=*), intent(in) :: rhs
    end function

    logical function charstring_lt_string( lhs, rhs )
      import String_t
      character(len=*), intent(in) :: lhs
      type(String_t),   intent(in) :: rhs
    end function

    logical function string_lt_string( lhs, rhs )
      import String_t
      type(String_t), intent(in) :: lhs
      type(String_t), intent(in) :: rhs
    end function
  end interface


  interface operator(<=)
    logical function string_le_charstring( lhs, rhs )
      import String_t
      type(String_t),   intent(in) :: lhs
      character(len=*), intent(in) :: rhs
    end function

    logical function charstring_le_string( lhs, rhs )
      import String_t
      character(len=*), intent(in) :: lhs
      type(String_t),   intent(in) :: rhs
    end function

    logical function string_le_string( lhs, rhs )
      import String_t
      type(String_t), intent(in) :: lhs
      type(String_t), intent(in) :: rhs
    end function
  end interface


  interface operator(>) 
    logical function string_gt_charstring( lhs, rhs )
      import String_t
      type(String_t),   intent(in) :: lhs
      character(len=*), intent(in) :: rhs
    end function

    logical function charstring_gt_string( lhs, rhs )
      import String_t
      character(len=*), intent(in) :: lhs
      type(String_t),   intent(in) :: rhs
    end function


    logical function string_gt_string( lhs, rhs )
      import String_t
      type(String_t), intent(in) :: lhs
      type(String_t), intent(in) :: rhs
    end function
  end interface


  interface operator(>=)
    logical function string_ge_charstring( lhs, rhs )
      import String_t
      type(String_t),   intent(in) :: lhs
      character(len=*), intent(in) :: rhs
    end function

    logical function charstring_ge_string( lhs, rhs )
      import String_t
      character(len=*), intent(in) :: lhs
      type(String_t),   intent(in) :: rhs
    end function

    logical function string_ge_string( lhs, rhs )
      import String_t
      type(String_t), intent(in) :: lhs
      type(String_t), intent(in) :: rhs
    end function
  end interface

  
  interface
    subroutine basestring_init_by_proto( ds, has_proto, proto )
      import String_t
      type(String_t), intent(inout) :: ds
      integer,        intent(in)    :: has_proto
      type(String_t), intent(in)    :: proto
    end subroutine
  end interface

!-----------------
  contains
!-----------------

  !_TypeGen_implementAll()


  subroutine charstring_assign_string_private( lhs, rhs )
    character(len=*), intent(out) :: lhs
    type(String_t),    intent(in) :: rhs
    call assign( lhs, rhs )
  end subroutine

  subroutine string_assign_string_private( lhs, rhs )
    type(String_t), intent(inout) :: lhs
    type(String_t),    intent(in) :: rhs
    call assign( lhs, rhs )
  end subroutine

  subroutine string_assign_charstring_private( lhs, rhs )
    type(String_t), intent(inout) :: lhs
    character(len=*),   intent(in)    :: rhs
    call assign( lhs, rhs )
  end subroutine

  subroutine string_assign_buf_private( lhs, rhs )
    type(String_t),                 intent(inout) :: lhs
    character(len=1), dimension(:), intent(in)    :: rhs
    call assign( lhs, rhs )
  end subroutine

end module

