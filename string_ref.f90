
module string_ref
  use, intrinsic :: iso_c_binding
  implicit none

  type, public :: StringRef
    type (c_ptr) :: loc
    integer*4    :: len
  end type

  interface str
    module procedure str_to_fstring
    module procedure fstring_to_str
  end interface

  abstract interface
    subroutine mainITF( argString )
      character(len=*) :: argString
    end subroutine
  end interface

!-------------------
  contains
!-------------------

  function str_to_fstring( fstr ) result(cstr)
    character(len=*), target, intent(in) :: fstr
    type (StringRef)                     :: cstr
    cstr%loc = c_loc(fstr)
    cstr%len = len(fstr)
  end function

  function fstring_to_str( cstr ) result(fptr)
    type (StringRef),     intent(in) :: cstr
    character(len=cstr%len), pointer :: fptr
    call c_f_pointer( cstr%loc, fptr )
  end function

end module

