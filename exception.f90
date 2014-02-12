
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

  contains

  function proc( sub ) result(res)
    use, intrinsic :: iso_c_binding
    procedure()     :: sub
    type (c_funptr) :: res
    res = boundProc( sub )

    contains

    function boundProc( sub ) result(res)
      interface
        subroutine VoidProc() bind(C); end subroutine
      end interface

      procedure(VoidProc) :: sub
      type (c_funptr)     :: res
      res = c_funloc(sub)
    end function
  end function

end module

