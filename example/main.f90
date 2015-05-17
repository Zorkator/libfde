
#include "exception.fpp"

module block_try
  use exception
  use string_ref
  use iso_c_binding
  implicit none

  integer*4 :: itr, x, y
  real*8    :: res
  real*8, dimension(:), pointer :: A => null(), B => null(), C => null()
  character(len=100) :: what

  contains

  recursive &
  subroutine main_prog()
    implicit none
    integer*4 :: loop, val
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
    implicit none
    integer*4 :: x
    call onError( proc(cleanup) )
    if (x < 0) &
      call throw( NotImplementedError, str("value lower zero") )
    if (x == 5) &
      call throw( ZeroDivisionError, str("value equals 5") )
    if (x >= 20) &
      call throw( OverflowError, str('value too large')  )
    res = 1.0/(x - 5)
    contains
    subroutine cleanup()
      print *, "func cleanup"
    end subroutine
  end function


  subroutine test_onError()
    implicit none
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

  recursive &
  subroutine hello_omp( n )
    implicit none
    integer*4 :: i, n, threadId, omp_get_thread_num
    !call omp_set_num_threads(4)
    
    allocate( A(n), B(n) )
    do i = 1, size(A)
      A(i) = i**2
    end do
    B = 0.0d0
    !$OMP PARALLEL num_threads(4) private(threadId)
      threadId = omp_get_thread_num()
      call calc_loop( threadId )
    !$OMP END PARALLEL
    deallocate( A, B )
  end subroutine

  recursive &
  subroutine calc_loop( threadId )
    implicit none
    integer*4 :: i, threadId

    print *, 'starting thread ', threadId
    _tryBlock(50)
      do i = 2, size(A)
        B(i) = (A(i) + A(i-1) - f(A(i))) / 2.0d0
      end do
    _tryCatch(50, (/ArithmeticError, RuntimeError/), what)
      case (ArithmeticError); print *, 'thread ', threadId, what
    _tryEnd(50)

  end subroutine

  recursive &
  function f( v )
    implicit none
    real*8 :: v, f
    real*8, dimension(:), pointer :: tmp
    allocate( tmp(int(v)) )
    f = v - 1.0 + g(v, tmp)
    deallocate( tmp )
  end function

  function g( v, buff )
    implicit none
    real*8 :: v, g
    real*8, dimension(:) :: buff
    if (v == 64.0) then
      call throw( ArithmeticError, str('this is kind of illegal!') )
    end if
    buff = v * 2.0 - 1.5
    g = sum( buff )
  end function

end module


program main
  use block_try

  call init_exception()
  call main_prog()
  call hello_omp( 200 )
  print *, "main finish"
  
end program

