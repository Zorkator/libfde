
#include "adt/itfUtil.fpp"

module adt_convert
  use iso_c_binding
  implicit none
  private

  interface hex
    module procedure hex_1, hex_2, hex_4, hex_8
  end interface

  interface address_str
    module procedure address_str_cptr
  end interface

  public :: hex, address_str

  contains

!_PROC_EXPORT(hex_1)
  function hex_1( val ) result(res)
    integer*1          :: val
    character(len=2+2) :: res
    write( res, 100 ) val
100 format('0x',Z2.2)
  end function
  
!_PROC_EXPORT(hex_2)
  function hex_2( val ) result(res)
    integer*2          :: val
    character(len=4+2) :: res
    write( res, 100 ) val
100 format('0x',Z4.4)
  end function
  
!_PROC_EXPORT(hex_4)
  function hex_4( val ) result(res)
    integer*4          :: val
    character(len=8+2) :: res
    write( res, 100 ) val
100 format('0x',Z8.8)
  end function
  
!_PROC_EXPORT(hex_8)
  function hex_8( val ) result(res)
    integer*8           :: val
    character(len=16+2) :: res
    write( res, 100 ) val
100 format('0x',Z16.16)
  end function


!_PROC_EXPORT(address_str_cptr)
  function address_str_cptr( cptr ) result(res)
    type(c_ptr)                           :: cptr
    character(len=storage_size(cptr)/4+2) :: res
    integer(kind=c_size_t)                :: address
    
    address = transfer( cptr, address )
    res = hex( address )
  end function



end module

