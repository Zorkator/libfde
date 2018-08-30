
module charstacker_impl__
  use iso_c_binding
  implicit none

# define _private
# include "adt/type/charstacker.ftype"

end module

!_PROC_EXPORT(charstacker_create_c)
subroutine charstacker_create_c( self, buffer, depth )
  use charstacker_impl__, only: CharStacker_t
  implicit none
  type(CharStacker_t)       :: self
  character(len=*),  target :: buffer
  integer*4,       optional :: depth
  integer*4                 :: depth_

  _optArg( depth_, depth, 32 )
  allocate( self%idx( depth_ ) )
  self%idx    = 0
  self%tos    = 0
  self%buffer => buffer
end subroutine


!_PROC_EXPORT(charstacker_push_c)
subroutine charstacker_push_c( self, what )
  use charstacker_impl__, only: CharStacker_t
  implicit none
  type(CharStacker_t)       :: self
  character(len=*)          :: what
  character(len=:), pointer :: tosPtr

  self%tos = self%tos + 1
  tosPtr   => self%buffer( self%idx( self%tos )+1: )
  tosPtr   = what
  self%idx( self%tos+1 ) = self%idx( self%tos ) + len(what)
end subroutine


!_PROC_EXPORT(charstacker_pop_c)
subroutine charstacker_pop_c( self )
  use charstacker_impl__, only: CharStacker_t
  implicit none
  type(CharStacker_t) :: self
  self%tos = self%tos - 1
end subroutine


!_PROC_EXPORT(charstacker_get_c)
function charstacker_get_c( self, lvl ) result(res)
  use charstacker_impl__, only: CharStacker_t
  implicit none
  type(CharStacker_t)       :: self
  integer*4,       optional :: lvl
  character(len=:), pointer :: res
  integer*4                 :: tos

  _optArg( tos, lvl, min(lvl, self%tos) )
  res => self%buffer( :self%idx(tos+1) )
end function

