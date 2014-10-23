
#include "adt/itfUtil.fpp"

module hash_code
  implicit none

  interface hash
    function hash_memory( byteArray, bytes ) bind(c,name='hash_memory_') result(code)
      use iso_c_binding
      integer(kind=c_size_t), intent(in) :: bytes
      integer(kind=c_int8_t), intent(in) :: byteArray(bytes)
      integer(kind=c_int32_t)            :: code, i
    end function

    function hash_string( str ) result(code)
      use iso_c_binding
      character(len=*), target, intent(in) :: str
      integer(kind=c_int32_t)              :: code
    end function
  end interface

end


  !PROC_EXPORT(hash_memory)
  function hash_memory( byteArray, bytes ) bind(c,name='hash_memory_') result(code)
    use iso_c_binding
    integer(kind=c_size_t), intent(in) :: bytes
    integer(kind=c_int8_t), intent(in) :: byteArray(bytes)
    integer(kind=c_int32_t)            :: code, i

    code = bytes
    do i = 1, bytes
      code = code + byteArray(i)
      code = code + code * (2**10)
      code = ieor( code, code / (2**6) )
    end do
    code = code + code * (2**3)
    code = ieor( code, code * (2**11) )
    code = code + code * (2**15)
  end function


  !PROC_EXPORT(hash_string)
  function hash_string( str ) result(code)
    use iso_c_binding
    character(len=*), target, intent(in) :: str
    integer(kind=c_int32_t)              :: code, hash_memory
    code = hash_memory( str(1:1), len_trim(str) )
  end function

