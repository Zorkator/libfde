
#include "fde/exception.fpp"

module test_exception
  use fde_exception
  use iso_c_binding
  implicit none

  contains

  subroutine tracer( op, skip, msg )
    procedure()       :: op
    integer*4         :: skip
    type(StringRef_t) :: msg
    print *, "<trace>, skipped frames:", skip
    print *, str(msg)
  end subroutine

  recursive &
  function reciprocal( valStr ) result(res)
    type(StringRef_t)         :: valStr
    real(8)                   :: res
    character(len=:), pointer :: valStrPtr
    integer                   :: stat

    res = 0.0
    valStrPtr => str(valStr)
    read(valStrPtr, *, iostat=stat) res
    if (stat /= 0) &
      call throw( ValueError, "unable to convert string '" // str(valStr) // "' to real!" )
    res = 1.0 / res
  end function

  recursive &
  subroutine test_conversion( valStr )
    type(StringRef_t) :: valStr
    real*8            :: res
    res = reciprocal( valStr )
    print *, res
  end subroutine

  subroutine test_pass_args()
    character(len=128) :: what
    character(len=10)  :: table(5)
    integer            :: idx, code

    data table /'2.0', '0.0', '-10', 'trash', '0.25'/
    do idx = 1, size(table)
      code = try( _catchAny,         what, test_conversion, table(idx) ) !< trace by preset tracer
      print *, '###'
      code = try( _catchAny, tracer, what, test_conversion, table(idx) ) !< trace by tracer
      print *, '###'
      code = try( _catchAny,               test_conversion, table(idx) ) !< no trace
      print *, '###'
    end do
    call throw( JobDone, "done" )
  end subroutine
end module

program testing
  use test_exception
  implicit none
  character(len=128) :: what

  call setup_standardExceptions()
! call set_traceproc( tracer ) !<
  select case (try( _catchAny, what, test_pass_args ))
    case     (0); continue
    case default; print *, trim(what)
  end select
  !call test_pass_args()
end program
