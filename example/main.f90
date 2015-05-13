
#include "exception.fpp"

module block_try
  use exception
  use string_ref
  use iso_c_binding
  implicit none

  integer*4 :: itr, x, y
  real*8    :: res

  contains

  recursive subroutine main_prog()
    integer*4 :: loop, val
    character(len=100) :: what
    type (StringRef) :: strRef
    type (c_ptr)     :: ptr
    integer(kind=c_long) :: xlen

    loop = 0
    val  = 1

    print *, "size: ", storage_size(strRef)/8
    print *, "size: ", storage_size(ptr)/8
    print *, "size: ", storage_size(xlen)/8


    !-- standard try block --
    _tryBlock(10)
      ! only in subroutines
      ! containing subroutines must be recursive!
      ! can't share local variables!
      res = func( itr + 42 )
      print *, itr, ": res = ", res
    _tryCatch(10, _catchAny, what)
      case default; print *, "caught exception: " // what
    _tryEnd(10)


    !-- nesting of try blocks --
    _tryBlock(11)
      print *, "entering outer block"

      _tryBlock(12)
        print *, "entering inner block"
        print *, func( -1 )

      _tryCatch(12, (/ArithmeticError/), what)
        case default; print *, "caught ArithmeticError: " // what
      _tryEnd(12)
      print *, "leaving outer block"

    _tryCatch(11, _catchAny, what)
      case default; print *, "caught exception: " // what
    _tryEnd(11)


    !-- try-do-while block --
    itr = 0
    _tryDo(21)
      itr = itr + 1
      res = func( itr )
      print *, itr, ": res = ", res
    _tryCatch(21, (/ArithmeticError/), what)
      ! just ignore
    _tryWhile(21, itr < 10)


    !-- use exception to break endless loop --
    itr = 10
    _tryBlock(22)
      do while (.true.)
        print *, func( itr )
        itr = itr + 1
      end do
    _tryCatch(22, (/OverflowError/), what)
    _tryEnd(22)


    !-- try-for-loop --
    _tryFor(30, itr = 20, itr > -5, itr = itr - 1)
      res = func( itr )
      print *, itr, ": res = ", res

      _tryFor(31, x = -1, x < 3, x = x + 1)
        print *, func( x )
      _tryCatch(31, _catchAny, what)
        case (NotImplementedError); continue
        case default;               _exitLoop(31) !<< only possible to exit inner loop!
      _tryEndFor(31)

    _tryCatch(30, (/ArithmeticError, RuntimeError/), what)
      case (RuntimeError); _exitLoop(30)
      case default;        continue
    _tryEndFor(30)

    _tryBlock(40)
      call test_onError()
    _tryCatch(40, _catchAny, what )
      case default; print *, "caught exception: " // what
    _tryEnd(40)

    print *, val, loop

  end subroutine


  real*8 function func( x ) result(res)
    integer*4 :: x
    if (x < 0) &
      call throw( NotImplementedError, str("value lower zero") )
    if (x == 5) &
      call throw( ZeroDivisionError, str("value equals 5") )
    if (x >= 20) &
      call throw( OverflowError, str('value too large')  )
    res = 1.0/(x - 5)
  end function


  subroutine test_onError()
    integer, dimension(:), pointer :: numArrayPtr => null()

    call onError( proc(cleanup) )
    
    allocate( numArrayPtr(5) )
    numArrayPtr = 42
    print *, numArrayPtr
    numArrayPtr(1) = func( numArrayPtr(2) )
    call cleanup()

  contains
    subroutine cleanup()
      print *, "cleanup: ", numArrayPtr
      deallocate( numArrayPtr )
    end subroutine
  end subroutine

end module


program main
  use block_try

  call main_prog()
  print *, "main finish"
  
end program

