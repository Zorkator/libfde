#define ASSERT(x) \
  if(.not. (x)) then ;\
    write(*,*) "Assertion failed in " // __FILE__ // ":", __LINE__ ;\
    call exit(1) ;\
  end if ;

program f_test
  use fde_basetypes
  use fde_typeinfo
  use fde_convert
  use fde_file
  use fde_crc, only: f_crc32 => crc32, crc32_file
  implicit none

  interface
    function crc32( crc, buf, size ) bind(c,name="crc32_bytebuffer_c_") result(crc_)
      use iso_c_binding
      integer(kind=c_int32_t), intent(in) :: crc
      integer(kind=c_size_t),  intent(in) :: size
      character(kind=c_char),  intent(in) :: buf(size)
      integer(kind=c_int32_t)             :: crc_
    end function
  end interface

  type(TypeInfo_t), pointer :: ti
  integer(kind=4)           :: i, j, code
  character(len=32), target :: msg

  msg = "this is a test-string" // char(10)

  ASSERT(hex( crc32( 0, msg(1:1), int(len_trim(msg), c_size_t) ) ) == "0x1225D297")
  ASSERT(hex( f_crc32( msg ) )                                     == "0x1225D297")
  ASSERT(hex( f_crc32( c_loc(msg), 0 ) )                           == "0x00000000")
  ASSERT(hex( f_crc32( c_loc(msg), len_trim(msg) ) )               == "0x1225D297")

! TODO: Will not work as fortran insists on using platform dependend line endings (CRLF/LF)
!# define _FILE      'crc_test_file.txt~'
!  i = fopen( _FILE )
!  write(i, '(A)') msg(:len_trim(msg)-1)
!  call close(i)
!  ASSERT(hex( crc32_file( _FILE ) )                                == "0x1225D297")
!  call close( fopen( _FILE ), status="delete" )

!?
!  ti  => type_of( len_trim(msg) )
!  print *, ti%typeId
!  print *, hex( f_crc32( c_loc(ti), storage_size(ti)/8 ) ) !< variable result


!?
!  do i = 1, 4000
!    do j = 1, 4000
!      code = f_crc32( msg )
!    end do
!  end do

end

