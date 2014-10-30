
module adt_crc
  implicit none
  private

  interface crc32
    function crc32_bytebuffer( seed, buf, size ) result(res)
      use iso_c_binding
      integer(kind=c_int32_t)             :: res
      integer(kind=c_int32_t), intent(in) :: seed
      integer(kind=c_size_t),  intent(in) :: size
      integer(kind=c_int8_t),  intent(in) :: buf(size)
    end function

    function crc32_string( str )
      use iso_c_binding
      integer(kind=c_int32_t)      :: crc32_string
      character(len=*), intent(in) :: str
    end function

    function crc32_c_ptr( cptr, size )
      use iso_c_binding
      integer(kind=c_int32_t)            :: crc32_c_ptr
      type (c_ptr),           intent(in) :: cptr
      integer(kind=c_size_t), intent(in) :: size
    end function
  end interface

  public :: crc32

end

