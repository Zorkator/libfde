
module adt_xtype_impl__
  use iso_c_binding
  implicit none

# define _private
# include "adt/prototype/xtype.fproto"

end module

!_PROC_EXPORT(xtype_create_by_xtype_c)
subroutine xtype_create_by_xtype_c( self, other )
  use adt_xtype_impl__, only: Xtype_t
  implicit none
  type(Xtype_t)           :: self
  type(Xtype_t), optional :: other

  if (present(other)) then
    self%ptr = other%ptr
    self%len = other%len
    self%alc = other%alc
    self%refstat = other%refstat
  end if
end subroutine


!_PROC_EXPORT(xtype_create_by_chrstr_c)
subroutine xtype_create_by_chrstr_c( self, chrstr )
  use adt_xtype_impl__, only: Xtype_t
  use iso_c_binding
  implicit none
  type(Xtype_t)      :: self
  character(len=*)   :: chrstr
  integer*1, dimension(:), pointer :: buff

  self%len = len(chrstr)
  if (self%len > 0) then
    call clone_()
  else
    self%ptr = C_NULL_PTR
  end if

contains
  subroutine clone_()
    character(len=self%len), pointer :: buff
    allocate( buff )
    buff = chrstr
    self%ptr  = c_loc(buff)
    self%alc  = self%len
  end subroutine
end subroutine

