
module encoders
  use generic_ref
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


program testinger
  use generic_ref
  use encoders
  use iso_c_binding
  implicit none

  interface
    subroutine bla(); end subroutine
  end interface

  integer*4, dimension(3,5), target  :: intArray
  integer*4, dimension(:,:), pointer :: ptr2d => null()
  integer*4, dimension(:)  , pointer :: ptr1d => null()
  integer*4,                 pointer :: ptr0d => null()
  !procedure(bla), dimension(:), pointer :: procPtr #< not possible to create array of proc pointers!
  procedure(func),       pointer :: f  => null()
  procedure(simpleCall), pointer :: sc => null()
  type(Ding)                     :: dong

  type(c_ptr)      :: cpointer
  type(GenericRef) :: ref1, ref2, ref3

  ref2 = ref(42)
  ref1 = ref(ref2)
  print *, int32(ref2)
  print *, int32(deref(ref1))

  if (is_ref(ref1)) then
    ref3 = deref(ref1)
    if (is_int32(ref3)) &
      print *, int32(ref3)
  end if

  ref3 = clone(ref1)
  print *, int32(deref(ref3))
  call free( ref3 )

  ref1 = ref(intArray)
  ptr2d => intXY(ref1)
  ptr2d = 42
  cpointer = cptr(ref1)

  ref2 = ref(4.23)
  ref1 = clone(ref2)

  call free(ref1)

  ref1 = ref(dong)

  ref2 = clone(ref1)

  call free(ref2)

  allocate( ptr2d(4,4) )
  ref1 = ref(ptr2d)

  ptr2d => null()
  ptr2d => intXY(ref1)

  ptr2d = 21

  ref2 = ref_from_CallBack( sub_a )
  print *, rank(ref2)
  print *, shape(ref2)
  ref2 = clone(ref2)

  sc => CallBack_from_ref( ref2 )
  call sc()

  ref2 = ref_from_CalcFunc( func_a )

  f => CalcFunc_from_ref( ref2 )
  print *, f( 2.43 )

  print *, shape(ref1)
  ref2 = clone(ref1)
  print *, shape(ref2)
  print *, intXY(ref2)

  call free(ref1)
  call free(ref2)
  call delete( ref1 )

end

