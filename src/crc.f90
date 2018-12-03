
module fde_crc
  implicit none
  private

  interface crc32
    function crc32_bytebuffer_c( seed, buf, size ) result(res)
      use iso_c_binding
      integer(kind=c_int32_t), intent(in) :: seed
      integer(kind=c_size_t),  intent(in) :: size
      integer(kind=c_int8_t),  intent(in) :: buf(size)
      integer(kind=c_int32_t)             :: res
    end function

    function crc32_string_c( str ) result(res)
      use iso_c_binding
      character(len=*), intent(in) :: str
      integer(kind=c_int32_t)      :: res
    end function

    function crc32_c_ptr( cptr, size ) result(res)
      use iso_c_binding
      type (c_ptr),           intent(in) :: cptr
      integer(kind=c_size_t), intent(in) :: size
      integer(kind=c_int32_t)            :: res
    end function
  end interface

  interface crc32_file
    function crc32_file_channel( chnl, len, seed, iostat ) result(res)
      use iso_c_binding
      integer(kind=c_int32_t), intent(in) :: chnl
      integer(kind=c_size_t)              :: len
      integer(kind=c_int32_t),   optional :: seed, iostat
      integer(kind=c_int32_t)             :: res
    end function

    function crc32_file_name( fileName, iostat, seed ) result(res)
      use iso_c_binding
      character(len=*),      intent(in) :: fileName
      integer(kind=c_int32_t), optional :: seed, iostat
      integer(kind=c_int32_t)           :: res
    end function
  end interface

  public :: crc32, crc32_file

end

