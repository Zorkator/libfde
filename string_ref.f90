
module string_ref
  use, intrinsic :: iso_c_binding
  implicit none

  type, public, bind(C) :: StringRef
    type (c_ptr)        :: loc
    integer(kind=c_int) :: len
  end type

  interface str
    module procedure str_to_stringRef, create_stringRef
    module procedure stringRef_to_str
  end interface

  interface char
    module procedure stringRef_to_char
  end interface

  interface len
    module procedure stringRef_len
  end interface

  interface
    subroutine mainITF( argString )
      character(len=*) :: argString
    end subroutine
  end interface

!-------------------
  contains
!-------------------

  function create_stringRef() result(strRef)
    type (StringRef) :: strRef
    strRef%loc = c_null_ptr
    strRef%len = 0
  end function

  function str_to_stringRef( fstr ) result(strRef)
    character(len=*), target, intent(in) :: fstr
    type (StringRef)                     :: strRef
    character(len=len(fstr)), pointer    :: ptr
    ptr => fstr
    strRef%loc = c_loc(ptr(1:1))
    strRef%len = len(fstr)
  end function

  function stringRef_to_str( strRef ) result(fptr)
    type (StringRef),       intent(in) :: strRef
    character(len=strRef%len), pointer :: fptr
    call c_f_pointer( strRef%loc, fptr )
  end function

  function stringRef_to_char( strRef ) result(cstr)
    type (StringRef), intent(in) :: strRef
    character(len=strRef%len)    :: cstr
    cstr = str(strRef)
  end function

  pure function stringRef_len( strRef ) result(l)
    type (StringRef), intent(in) :: strRef
    integer*4                    :: l
    l = strRef%len
  end function

end module

