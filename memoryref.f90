
#include "adt/itfUtil.fpp"

module adt_memoryref
  use, intrinsic :: iso_c_binding
  implicit none
  private
  
  type, public, bind(c) :: MemoryRef_t
    type (c_ptr)           :: loc = C_NULL_PTR
    integer(kind=c_size_t) :: len = 0
  end type

!---------------
  contains
!---------------

!_PROC_EXPORT(memoryref_object_size)
  integer(kind=c_size_t) &
  function memoryref_object_size() result(res)
    type (MemoryRef_t) :: tmp
    res = storage_size(tmp) / 8
  end function

!_PROC_EXPORT(memoryref_init)
  subroutine memoryref_init( self )
    type(MemoryRef_t), intent(inout) :: self
    self%loc = C_NULL_PTR
    self%len = 0
  end subroutine

end module

