
#define _catch(cases)   [cases, 0]
#define _catchAny       [0]


module funx
  use exception_base

  contains

  subroutine another( msg1, msg2, val1 )!, val2, rval )
    type (StringRef) :: msg1, msg2
    integer*4        :: val1!, val2
    !real*8    :: rval
    print *, str(msg1)
    !if (val1 + val2 < rval) then
      !val2 = val1
    !else
      call throwException(val1)
    !endif
    print *, str(msg2)
  end subroutine

  subroutine mayFail( val, msg )
    integer*4        :: val
    type (StringRef) :: msg

    if (val < 10) then
      call throwException(1)
    else
      print *, str(msg)
      val = val / 2

      print *, tryCall( _catch((/1/)), another, str("bla"), str("& text"), val )!, val, 4.2 )
        !case (0); print *, "catched"
      !end select
    endif
  end subroutine

end module

program main
  use funx
  use exception_base
  implicit none
  integer*4 :: val

  val = 2
  select case ( tryCall( _catchAny, mayFail, val, str("test") ) )
    case (1); print *, "catched exception 1"
    case (2); print *, "catched exception 2"
    case (3); print *, "catched exception 3"
    case default; print *, "ok"
  end select

  print *, "finish"

end program

