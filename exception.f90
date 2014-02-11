
#include "exception.fpp"

module arg_terminator
  implicit none

  ! We have to split this apart from exception_base, since
  !  fortran does not allow to use any encompassing module

  type NeverProvideThis
    ! empty
  end type
end module

module exception
  implicit none

  interface try
    _tryProcedure( exception_base_tryCall_0__, _0_args )
    _end_tryProcedure
  end interface

  interface
    subroutine throw( code ) bind(C,name="f_throw")
    use, intrinsic :: iso_c_binding
    integer(kind=c_int), value :: code
    end subroutine
  end interface

end module

