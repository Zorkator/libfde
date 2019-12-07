
#include "fde/itfUtil.fpp"

module fde_memoryref
  use, intrinsic :: iso_c_binding
  implicit none
  public
  
  type, bind(c) :: MemoryRef_t
    type (c_ptr)           :: loc = C_NULL_PTR
    integer(kind=c_size_t) :: len = 0
  end type

  type(MemoryRef_t), parameter :: null_ref = MemoryRef_t( C_NULL_PTR, 0 )

!---------------
  contains
!---------------

!_PROC_EXPORT(memoryref_object_size_c)
  integer(kind=c_size_t) &
  function memoryref_object_size_c() result(res)
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

