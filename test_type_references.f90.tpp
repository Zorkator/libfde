
module type_references
  use adt_ref
  use adt_list
  implicit none
  private

  abstract interface
    function func( x ) result(res)
      real*4 :: x, res
    end function
  end interface

  type, public :: Ding
    integer*4 :: val1, val2
    real*4    :: val3
  end type

  public :: func, sub_a, func_a

  !_TypeGen_declare_ListNode( public, intXY, integer*4, dimension(5,4) )

  !_TypeGen_declare_RefType( public, intXY,    integer*4, dimension(:,:) )
  !_TypeGen_declare_RefType( public, CalcFunc, procedure(func),  scalar )
  !_TypeGen_declare_RefType( public, ADing, type(Ding),  scalar, cloneMode = _type, deleteProc = ding_clear )

  contains

  !_TypeGen_implementAll()

  subroutine ding_clear( self )
    type(Ding) :: self
    print *, 'ding_clear'
    self%val1 = 0
    self%val2 = 0
    self%val3 = 0.0
  end subroutine

  subroutine sub_a()
    print *, "sub_a was called"
  end subroutine

  function func_a( x ) result(y)
    real*4 :: x, y
    y = x ** 2
  end function

end module

