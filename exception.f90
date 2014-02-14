
#include "exception.fpp"

module exception
  use iso_c_binding
  implicit none

  integer*4, parameter :: StopExecution       = x'01000000'

  integer*4, parameter :: StandardError       = x'02000000'

  integer*4, parameter :: ArithmeticError     = x'02010000'
  integer*4, parameter :: ZeroDivisionError   = x'02010100'
  integer*4, parameter :: OverflowError       = x'02010200'
  integer*4, parameter :: FloatingPointError  = x'02010400'

  integer*4, parameter :: AssertionError      = x'02020000'

  integer*4, parameter :: EnvironmentError    = x'02040000'
  integer*4, parameter :: IOError             = x'02040100'

  integer*4, parameter :: EOFError            = x'02080000'

  integer*4, parameter :: MemoryError         = x'02100000'

  integer*4, parameter :: RuntimeError        = x'02200000'
  integer*4, parameter :: NotImplementedError = x'02200100'

  integer*4, parameter :: ValueError          = x'02400000'


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

