
module test_file
  use fde_file
  implicit none

  character(len=255) :: buffer
  integer            :: fh(5), i

contains

  subroutine test_open_close()
    character(10) :: files(5) = ["test.1.txt", "test.2.txt", "test.3.txt", "test.4.txt", "test.5.txt"]

    fh = fopen( files, position='append' )
    print *, file_exists( files )

    do i = 1, size(files)
      write(*, *) i, file_name( fh(i), buffer )
    end do

    call close( fh, status="delete" )
    print *, file_exists( files )
    print *, file_size( __FILE__ )
  end subroutine

end module

program testing
  use test_file 

  call test_open_close()
end program

