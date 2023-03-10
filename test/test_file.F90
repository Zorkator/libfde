
#include "fde/exception.fpp"

module test_file
  use fde_file
  use fde_string
  use fde_exception
  implicit none

  character(len=255) :: buffer
  integer            :: fh(5), i

contains

  recursive &
  subroutine test_open_close()
    character(10) :: files(5) = ["test.1.txt", "test.2.txt", "test.3.txt", "test.4.txt", "test.5.txt"]

    ! explicit close of non-opened units
    call close( [10,20,30,40], iostat=i )

    fh = fopen( files, position='append' )
    _assert( all( file_exists( files ) ) )

    do i = 1, size(files)
      _assert( filename( file_name( fh(i), buffer ) ) == files(i) )
    end do

    call close( fh, status="delete" )
    _assert( all( file_exists( files ) .eqv. .false. ) )


    fh = fopen( files, position='append', unitrange=[10,20] )
    _assert( all(fh == (/10,11,12,13,14/)) )
    call close( fh, status="delete" )


#   define _testfile 'size_test.bj'

    fh(1) = fopen( _testfile, form='UNFORMATTED', action='WRITE' )
    write( fh(1) ) 42
    call close( fh(1) )
    _assert( file_exists( _testfile ) )
    _assert( file_size( _testfile ) == 12 )

    call close( fopen( _testfile ), status="delete" )
    _assert( .not. file_exists( _testfile ) )

    !-- coverage-flag causes gfortran to fail on compiling tryBlock-variant...
    !_tryBlock(1)
    !  call open( -12, _testfile, form="UNFORMATTED", action='WRITE' )
    !_tryCatch_nt(1, IOError)
    !  case (0); call throw( RuntimeError, 'expected IOError not thrown!' )
    !_tryEnd(1)
    !
    select case( try( _catchAny, open_bad_unit ) )
      case (IOError); continue !< caught expected exception
      case   default; call throw( RuntimeError, 'expected IOError not thrown!' )
    end select

    contains

    subroutine open_bad_unit()
      ! Opening a bad unit number with the open-statement would terminate the program.
      ! The fde_file-open is expected to throw a IOError instead!
      call open( -12, _testfile, form="UNFORMATTED", action='WRITE' )
    end subroutine

  end subroutine

end module

program testing
  use test_file

  call test_open_close()
end program

