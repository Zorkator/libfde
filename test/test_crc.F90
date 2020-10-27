
#include "fde/exception.fpp"

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

  integer(kind=4)           :: i
  character(len=32), target :: msg, tagged
  character(len=4), pointer :: tag
  integer(kind=4),   target :: msgLen

  msg    = "this is a test-message"
  msgLen = len_trim(msg)
  call c_f_pointer( c_loc(msgLen), tag )
  tagged = tag // trim(msg) // tag  !< add length tags fortran writes to UNFORMATTED file 

  _assert( hex( crc32( 0, tagged(1:1), int(len_trim(tagged), c_size_t) ) ) == '0x946614C8' )
  _assert( hex( f_crc32( tagged ) )                                        == '0x946614C8' )
  _assert( hex( f_crc32( c_loc(tagged), 0 ) )                              == '0x00000000' )
  _assert( hex( f_crc32( c_loc(tagged), len_trim(tagged) ) )               == '0x946614C8' )

# define _FILE      'crc_test_file.txt~'
  i = fopen( _FILE, 'UNFORMATTED' )
  write(i) trim(msg)
  call close(i)
  _assert( hex( crc32_file( _FILE ) )                                      == '0x946614C8' )
  call close( fopen( _FILE ), status="delete" )

end

