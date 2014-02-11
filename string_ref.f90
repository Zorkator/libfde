
module string_ref
  use, intrinsic :: iso_c_binding
  implicit none

  type, public, bind(C) :: StringRef
    type (c_ptr) :: loc = c_null_ptr
    integer*4    :: len = 0
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

  abstract interface
    subroutine mainITF( argString )
      character(len=*) :: argString
    end subroutine
  end interface

!-------------------
  contains
!-------------------

  function create_stringRef() result(strRef)
    type (StringRef) :: strRef
  end function

  function str_to_stringRef( fstr ) result(strRef)
    character(len=*), target, intent(in) :: fstr
    type (StringRef)                     :: strRef
    strRef%loc = c_loc(fstr)
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

