
module fde_charstacker
  implicit none
  private

# include "fde/type/charstacker.ftype"

  interface CharStacker
    module procedure create_
  end interface

  public :: CharStacker
  public :: initialize, delete, push, pop, get, size, len

contains

!_PROC_EXPORT(create_)
  function create_( buffer, depth ) result(self)
    character(len=*),  target :: buffer
    integer*4,       optional :: depth
    type(CharStacker_t)       :: self
    call initialize( self, buffer, depth )
  end function
end module

