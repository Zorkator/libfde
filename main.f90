
#include "exception.fpp"

!{{{
#if 0
module funx
  use exception, only : throw
  use string_ref

  interface try
    _tryProcedure( tryFunc_1, _args_5 )
      import StringRef
      integer(kind=c_int) :: arg1
      type (StringRef)    :: arg2
    _end_tryProcedure

    !_tryProcedure( tryFunc_2, 3 )
    !  import StringRef
    !  type (StringRef)    :: arg1, arg2
    !  integer(kind=c_int) :: arg3
    !_end_tryProcedure
  end interface

  contains

  subroutine another( msg1, msg2, val1 )!, val2, rval )
    type (StringRef) :: msg1, msg2
    integer*4        :: val1!, val2
    !real*8    :: rval
    print *, str(msg1)
    !if (val1 + val2 < rval) then
      !val2 = val1
    !else
      call throw(val1)
    !endif
    print *, str(msg2)
  end subroutine

  subroutine mayFail( val, msg )
    integer*4        :: val
    type (StringRef) :: msg

    if (val < 10) then
      call throw(1)
    else
      print *, str(msg)
      val = val / 2

      !print *, try( _catch((/1/)), proc(another), str("bla"), str("& text"), val, _argEnd )!, val, 4.2 )
        !case (0); print *, "catched"
      !end select
    endif
  end subroutine

  subroutine entryTest( x, y )
    integer*4 :: x, y

    print *, "entryTest begin ", x + y
    y = x+1
  entry testMiddle( x )
    print *, "entryTest middle ", x
  entry testEnd()
    print *, "entryTest end "

  end subroutine

  recursive subroutine main_prog()
    use exception, only : proc, throw, IOError
    implicit none
    integer*4 :: val, val2, code, v
    type(StringRef) :: s

    val = 42; val2 = 21
    call entryTest( val, val2 )
    call testMiddle( val )
    call testEnd()

    val = 2
    select case ( try( _catchAny, proc(mayFail), val, str("test"), _argEnd ) )
      case (1); print *, "catched exception 1"
      case (2); print *, "catched exception 2"
      case (3); print *, "catched exception 3"
      case default; print *, "ok"
    end select

    code = try( _catchAny, proc(block), val, str("block_call"), _argEnd )
    goto 99
    entry block( v, s )
      print *, "block was here", v, str(s)
      call throw( IOError )
      return
 99 select case (code)
      case (0); print *, "ok"
      case default; print *, "catched something"
    end select

    print *, "main_prog finish"
  end subroutine

end module
#endif
!}}}

module block_try
  use exception

  contains

  recursive subroutine main_prog()
    integer*4 :: code, loop, val

#   define _tryBlock(label) \
      goto label; entry _paste(tryblock__,label)

#   define _tryCatch(label, catchList)  \
      return                           ;\
      label select case( try( _catch(catchList), proc(_paste(tryblock__,label)), _argEnd ) )

#   define _tryEnd \
      end select

# define _tryDo(label) \
    _paste(label,00) _tryBlock(label)

# define _tryWhile(label,cond) \
    _tryEnd                   ;\
    if (cond) goto _paste(label,00)

    loop = 1
    _tryDo(42)
      ! only in subroutines
      ! containing subroutines must be recursive!
      ! can't share local variables!
      print *, "enter value"
      read(5,*), val !<< different instance of variable code!!!

      print *, "testinger"
      select case (val)
        case (10);    call throw( IOError )
        case (20);    call throw( ZeroDivisionError )
        case default; code = func( val )
      end select

    _tryCatch(42, (/ArithmeticError, EnvironmentError/))
      case (ArithmeticError);  print *, "catched ArithmeticError"
      case (EnvironmentError); print *, "catched EnvironmentError"
      case default;            loop = 0
    _tryWhile(42, loop .ne. 0)

    print *, code, loop
  end subroutine

  integer*4 function func( x ) result(res)
    integer*4 :: x, y
    res = 1.0/(x - 2.0)
  end function

  subroutine fpe_handler( sig, code )
    !use ifport
    integer :: sig, code, ir
    character*10 :: out, in
    ir = ieee_flags('clear', 'exception', 'division', out)
    call throw( ZeroDivisionError )
  end subroutine

end module


program main
  use block_try

  integer :: ir

  ir = ieee_handler('set', 'division', fpe_handler)
  call main_prog()
  print *, "main finish"
  
end program

