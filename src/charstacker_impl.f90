
module charstacker_impl__
  use iso_c_binding
  implicit none

# define _private
# include "fde/type/charstacker.ftype"

end module

!_PROC_EXPORT(charstacker_create_c)
subroutine charstacker_create_c( self, buffer, depth )
  use charstacker_impl__, only: CharStacker_t, delete
  implicit none
  type(CharStacker_t)       :: self
  character(len=*),  target :: buffer
  integer*4,       optional :: depth

  call delete( self )
  _optArg( self%depth, depth, _MAXDEPTH )
  allocate( self%idx( self%depth ) )
  self%idx    = 0
  self%buffer => buffer
end subroutine


!_PROC_EXPORT(charstacker_delete_c)
subroutine charstacker_delete_c( self )
  use charstacker_impl__, only: CharStacker_t
  implicit none
  type(CharStacker_t) :: self
  
  if (allocated( self%idx )) then
    deallocate( self%idx )
  end if
  nullify( self%buffer )
  self%depth = 0
  self%tos   = 0
end subroutine


!_PROC_EXPORT(charstacker_push_c)
subroutine charstacker_push_c( self, what )
  use charstacker_impl__, only: CharStacker_t
  implicit none
  type(CharStacker_t)       :: self
  character(len=*)          :: what
  character(len=:), pointer :: tosPtr

  if (self%tos < self%depth) then
    self%tos = self%tos + 1
    tosPtr   => self%buffer( self%idx( self%tos )+1 : len(self%buffer) )
    tosPtr   = what
    self%idx( self%tos+1 ) = self%idx( self%tos ) + len(what)
  end if
end subroutine


!_PROC_EXPORT(charstacker_pop_c)
subroutine charstacker_pop_c( self )
  use charstacker_impl__, only: CharStacker_t
  implicit none
  type(CharStacker_t) :: self

  if (self%tos > 0) then
    self%tos = self%tos - 1
  end if
end subroutine


!_PROC_EXPORT(charstacker_get_c)
function charstacker_get_c( self ) result(res)
  use charstacker_impl__, only: CharStacker_t
  implicit none
  type(CharStacker_t)       :: self
  character(len=:), pointer :: res
  integer*4                 :: tos

  res => self%buffer( :self%idx(self%tos+1) )
end function


!_PROC_EXPORT(charstacker_size_c)
function charstacker_size_c( self ) result(res)
  use charstacker_impl__, only: CharStacker_t
  implicit none
  type(CharStacker_t) :: self
  integer*4           :: res

  res = self%tos
end function


!_PROC_EXPORT(charstacker_length_c)
function charstacker_length_c( self ) result(res)
  use charstacker_impl__, only: CharStacker_t
  implicit none
  type(CharStacker_t) :: self
  integer*4           :: res

  res = self%idx(self%tos+1)
end function

