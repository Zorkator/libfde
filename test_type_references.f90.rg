
module type_references
  use generic_ref
  use type_info
  implicit none
  private

  abstract interface
    subroutine simpleCall(); end subroutine
  end interface

  abstract interface
    function func( x ) result(res)
      real*4 :: x, res
    end function
  end interface

  type, public :: Ding
    integer*4 :: val1, val2
    real*4    :: val3
  end type

  public :: simpleCall, func, sub_a, func_a

  !_TypeReference_declare( public, int32,    integer*4, scalar )
  !_TypeReference_declare( public, intXY,    integer*4, dimension(:,:) )
  !_TypeReference_declare( public, real32,   real*4,    scalar, cloneProc = float_cloner, deleteProc = float_clear )
  !_TypeReference_declare( public, CallBack, procedure(simpleCall),  scalar )
  !_TypeReference_declare( public, CalcFunc, procedure(func),  scalar )
  !_TypeReference_declare( public, ADing, type(Ding),  scalar )

  contains

  !_TypeReference_implementAll()

  function float_cloner( val ) result(res)
    real*4, intent(in) :: val
    real*4,    pointer :: res
    print *, 'float_cloner'
    allocate( res )
    res = val
  end function

  subroutine float_clear( val )
    real*4 :: val
    print *, 'float_clear'
    val = 0.0
  end subroutine


  subroutine sub_a()
    print *, "sub_a was called"
  end subroutine

  function func_a( x ) result(y)
    real*4 :: x, y
    y = x ** 2
  end function

end module

