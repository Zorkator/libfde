
module fde_charstacker
  implicit none

# include "fde/type/charstacker.ftype"

  interface CharStacker
    module procedure create_
  end interface

contains

!_PROC_EXPORT(create_)
  function create_( buffer, depth ) result(self)
    character(len=*),  target :: buffer
    integer*4,       optional :: depth
    type(CharStacker_t)           :: self
    call charstacker_create_c( self, buffer, depth )
  end function
end module

