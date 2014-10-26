
program f_test
  use iso_c_binding
  use crc, only: f_crc32 => crc32
  implicit none

  interface
    function crc32( crc, buf, size ) bind(c,name="crc32") result(crc_)
      use iso_c_binding
      integer(kind=c_int32_t), value, intent(in) :: crc
      integer(kind=c_size_t),  value, intent(in) :: size
      character(kind=c_char),         intent(in) :: buf(size)
      integer(kind=c_int32_t)                    :: crc_
    end function
  end interface
  
  integer*4 :: i, j, code
  character(len=32) :: str
  str = "this is a test-string"
  print *, crc32( 0, str(1:1), len_trim(str) )
  print *, f_crc32( str )
  print *, f_crc32( c_loc(str), len_trim(str) )
  print *, f_crc32( c_loc(code), storage_size(code)/8 )


  do i = 1, 4000
    do j = 1, 4000
      code = f_crc32( str )
    end do
  end do

end

