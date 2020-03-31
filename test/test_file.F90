
#include "fde/exception.fpp"

module test_file
  use fde_file
  use fde_exception
  implicit none

  character(len=255) :: buffer
  integer            :: fh(5), i

contains

  subroutine test_open_close()
    character(10) :: files(5) = ["test.1.txt", "test.2.txt", "test.3.txt", "test.4.txt", "test.5.txt"]

    fh = fopen( files, position='append' )
    _assert( file_exists( files ) )

    do i = 1, size(files)
      _assert( file_name( fh(i), buffer ) == files(i) )
    end do

    call close( fh, status="delete" )
    _assert( file_exists( files ) == .false. )

#   define _testfile 'size_test.bj'

    fh(1) = fopen( _testfile, form='UNFORMATTED' )
    write( fh(1), * ) 42
    call close( fh(1) )
    _assert( file_size( _testfile ) == 12 )
  end subroutine

end module

program testing
  use test_file 

  call test_open_close()
end program

