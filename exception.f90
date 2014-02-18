
#include "exception.fpp"

module exception
  use iso_c_binding
  implicit none

  ! Predefinition of hierarchical exception types.
  ! Hierarchical means that catching a certain type of Error
  !   also catches it's subtypes.
  ! The indentation indicates the relation of different errors,
  !   which is encoded by the assigned parameter value.

  integer*4, parameter :: StopExecution           = x'01000000'

  integer*4, parameter :: StandardError           = x'02000000'

  integer*4, parameter ::   ArithmeticError       = x'02010000'
  integer*4, parameter ::     ZeroDivisionError   = x'02010100'
  integer*4, parameter ::     OverflowError       = x'02010200'
  integer*4, parameter ::     FloatingPointError  = x'02010400'

  integer*4, parameter ::   AssertionError        = x'02020000'
                            
  integer*4, parameter ::   EnvironmentError      = x'02040000'
  integer*4, parameter ::     IOError             = x'02040100'
                            
  integer*4, parameter ::   EOFError              = x'02080000'
                            
  integer*4, parameter ::   MemoryError           = x'02100000'
                            
  integer*4, parameter ::   RuntimeError          = x'02200000'
  integer*4, parameter ::     NotImplementedError = x'02200100'
                            
  integer*4, parameter ::   ValueError            = x'02400000'


  ! Using the try-catch mechanism requires an interface definition
  !   matching the subroutine to call via try.
  ! The following defines the default interface for trying any
  !   subroutine without arguments.
  ! For trying any other subroutine it's signature has to be defined
  !   manually.
  ! The preprocessor macros _tryProcedure() and _end_tryProcedure
  !   help to define them correctly.
  ! Keep in mind that such interface is used to call a C-function.
  ! For this reason it is important to use the type kinds provided by
  !   fortran's iso_c_binding module for specifying the argument types.
  ! As there's no standard for passing fortran strings to C the module string_ref
  !   defines a wrapper type StringRef that can handle strings portably.
  !
  ! Definding the interface for a subroutine taking three arguments,
  !   e.g. an integer, a real, and a string, might look like this:
  !
  ! interface try
  !   _tryProcedure( some_unique_name_, 3 ) !<< number of expected arguments
  !     integer(kind=c_int) :: arg1   !<< define arguments by dummy names arg#
  !     real(kind=c_double) :: arg2   !<< ... starting at arg1, up to arg20
  !     type(StringRef)     :: arg3
  !   _end_tryProcedure
  ! end interface
  !

  interface try
    _tryProcedure( exception_try_0_args_, 0 )
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

