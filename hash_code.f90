
#include "adt/itfUtil.fpp"

module hash_code
  implicit none

  interface hash; module procedure hash_memory, hash_cmemory, hash_string; end interface

  contains

  !PROC_EXPORT_2REF( hash_memory, byteArray, bytes )
  function hash_memory( byteArray, bytes ) result(code)
    integer,   intent(in) :: bytes
    integer*1, intent(in) :: byteArray(bytes)
    integer               :: code, i

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


  !PROC_EXPORT_2REF( hash_cmemory, cptr, bytes )
  function hash_cmemory( cptr, bytes ) result(code)
    use iso_c_binding
    type (c_ptr),         intent(in) :: cptr
    integer,              intent(in) :: bytes
    integer                          :: code
    integer*1, dimension(:), pointer :: dataPtr

    call c_f_pointer( cptr, dataPtr, (/bytes/) )
    code = hash_memory( dataPtr, bytes )
  end function


  !PROC_EXPORT_1REF( hash_string, str )
  function hash_string( str ) result(code)
    use iso_c_binding
    character(len=*), target, intent(in) :: str
    integer                              :: code
    code = hash_cmemory( c_loc(str), len_trim(str) )
  end function

end

