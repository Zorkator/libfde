
#include "fortres/exception.fpp"
#include "fortres/exception.fmod"

module block_try
  use exception
  use iso_c_binding
  implicit none

  integer*4          :: itr, x, y
  real*8             :: res
  character(len=100) :: what

  contains

  recursive subroutine main_prog()
    integer*4 :: loop, val
    type (StringRef_t) :: strRef
    type (c_ptr)     :: ptr
    integer(kind=c_long) :: xlen

    loop = 0
    val  = 1
    what = ''

    print *, "size: ", storage_size(strRef)/8
    print *, "size: ", storage_size(ptr)/8
    print *, "size: ", storage_size(xlen)/8


    !-- standard try block --
!    _tryBlock(10)
!      ! only in subroutines
!      ! containing subroutines must be recursive!
!      ! can't share local variables!
!      res = func( itr + 42 )
!      print *, itr, ": res = ", res
!    _tryCatch(10, _catchAny, what)
!      case default; print *, "catched exception: " // what
!    _tryEnd(10)


!    !-- nesting of try blocks --
!    _tryBlock(11)
!      print *, "entering outer block"
!
!      _tryBlock(12)
!        print *, "entering inner block"
!        print *, func( -1 )
!
!      _tryCatch(12, (/ArithmeticError/), what)
!        case default; print *, "catched ArithmeticError: " // what
!      _tryEnd(12)
!      print *, "leaving outer block"
!
!    _tryCatch(11, _catchAny, what)
!      case default; print *, "catched exception: " // what
!    _tryEnd(11)
!
!
    !-- try-do-while block --
    itr = 0
    _tryDo(21)
      itr = itr + 1
      res = func( itr )
      print *, itr, ": res = ", res
    _tryCatch(21, (/ArithmeticError/), what)
      print *, what
    _tryWhile(21, itr < 10)
!
!
!    !-- use exception to break endless loop --
!    itr = 10
!    _tryBlock(22)
!      do while (.true.)
!        print *, func( itr )
!        itr = itr + 1
!      end do
!    _tryCatch(22, (/OverflowError/), what)
!    _tryEnd(22)
!
!
!    !-- try-for-loop --
!    _tryFor(30, itr = 20, itr > -5, itr = itr - 1)
!      res = func( itr )
!      print *, itr, ": res = ", res
!
!      _tryFor(31, x = -1, x < 3, x = x + 1)
!        print *, func( x )
!      _tryCatch(31, _catchAny, what)
!        case (NotImplementedError); continue
!        case default;               _exitLoop(31) !<< only possible to exit inner loop!
!      _tryEndFor(31)
!
!    _tryCatch(30, (/ArithmeticError, RuntimeError/), what)
!      case (RuntimeError); _exitLoop(30)
!      case default;        continue
!    _tryEndFor(30)

    print *, val, loop

  end subroutine


  subroutine test_proc()
    select case (try( _catchAny, str(what), proc(level_1), _argEnd ))
      case (0)    ; continue
      case default; print *, what
    end select
  end subroutine


  subroutine level_1()
    call push_cleanup( proc( cleanup ) )
    select case (try( _catch(NotImplementedError), str(what), proc(level_2), _argEnd ))
      case default; continue
    end select
    call pop_cleanup( 0 )
    contains

    subroutine cleanup()
      print *, "cleanup level_1"
    end subroutine
  end subroutine


  subroutine level_2()
    call push_cleanup( proc( cleanup ) )
    select case (try( _catch(ZeroDivisionError), str(what), proc(level_3), _argEnd ))
      case default; continue
    end select
    call pop_cleanup( 0 )
    
    contains

    subroutine cleanup()
      print *, "cleanup level_2"
    end subroutine
  end subroutine


  subroutine level_3()
    call push_cleanup( proc( cleanup ) )
    select case (try( _catch(OverflowError), str(what), proc(level_4), _argEnd ))
      case default; continue
    end select
    call pop_cleanup( 0 )
    
    contains

    subroutine cleanup()
      print *, "cleanup level_3"
    end subroutine
  end subroutine


  subroutine level_4()
    call push_cleanup( proc( cleanup ) )
    select case (try( _catch(TypeError), str(what), proc(level_5), _argEnd ))
      case default; continue
    end select
    call pop_cleanup( 0 )
    
    contains

    subroutine cleanup()
      print *, "cleanup level_4"
    end subroutine
  end subroutine


  subroutine level_5()
    integer*4 :: counter = -2

    call push_cleanup( proc( cleanup ) )
    counter = counter + 1
    print *, func(counter)
    call pop_cleanup( 0 )
    
    contains

    subroutine cleanup()
      print *, "cleanup level_5"
    end subroutine
  end subroutine


  real*8 function func( x ) result(res)
    integer*4 :: x
    if (x < 0) &
      call _throw( NotImplementedError, "value lower zero" )
    if (x == 5) &
      call _throw( ZeroDivisionError, "value equals 5" )
    if (x >= 7) &
      call _throw( OverflowError, 'value too large' )
    res = 1.0/(x - 5)
  end function

end module


program main
  use block_try

  call main_prog()
  print *, "main finish"
  call test_proc(); print *, what
  call test_proc(); print *, what
  call test_proc(); print *, what
  call test_proc(); print *, what
  call test_proc(); print *, what
  call test_proc(); print *, what
  call test_proc(); print *, what
  call test_proc(); print *, what
  call test_proc(); print *, what
  call test_proc(); print *, what
  
end program

