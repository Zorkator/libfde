
#include "exception.fpp"

module exception
  use iso_c_binding
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

  interface proc
    module procedure anyProc_to_tryProc
  end interface

  contains

  function anyProc_to_tryProc( proc ) result(res)
    use, intrinsic :: iso_c_binding
    procedure()     :: proc
    type (c_funptr) :: res
    res = c_funloc(proc)
  end function

end module

