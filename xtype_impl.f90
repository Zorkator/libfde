
module xtype_impl__
  use iso_c_binding
  implicit none

# define _private
# include "adt/type/xtype.ftype"

  character(len=0), target :: empty_string_ = ''

end module

!_PROC_EXPORT(xtype_create_by_xtype_c)
subroutine xtype_create_by_xtype_c( self, other )
  use xtype_impl__, only: Xtype_t
  implicit none
  type(Xtype_t)           :: self
  type(Xtype_t), optional :: other

  if (present(other)) then
    self%ptr = other%ptr
    self%len = other%len
    self%alc = other%alc
    !self%refstat = other%refstat
  end if
end subroutine


!_PROC_EXPORT(xtype_create_by_chrstr_c)
subroutine xtype_create_by_chrstr_c( self, chrstr )
  use xtype_impl__, only: Xtype_t
  use iso_c_binding
  implicit none
  type(Xtype_t)    :: self
  character(len=*) :: chrstr

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


!_PROC_EXPORT(xtype_delete_c)
subroutine xtype_delete_c( self )
  use xtype_impl__, only: Xtype_t, c_f_pointer
  implicit none
  type(Xtype_t)             :: self
  character(len=:), pointer :: buff

  !self%len = 0; self%alc = 0
  !if (_ref_isMine( self%refstat )) then
  !  call c_f_pointer( self%ptr, buff, [self%len] )
  !  deallocate( buff )
  !  _ref_setMine( self%refstat, 0 )
  !end if
end subroutine


!_PROC_EXPORT(xtype_ptr_f)
function xtype_ptr_f( self ) result(res)
  use xtype_impl__, only: Xtype_t
  implicit none
  type(Xtype_t)             :: self
  character(len=:), pointer :: res

  !if (_ref_isWeakMine( self%refstat )) then
  !  res => empty_string_
  !  call xtype_delete_c( self )
  !else
  !  call c_f_pointer( self%ptr, res, [self%len] )
  !end if
end function


!_PROC_EXPORT(xtype_cptr_f)
function xtype_cptr_f( self ) result(res)
  type(Xtype_t) :: self
  type(c_ptr)   :: res

  !if (_ref_isWeakMine( self%refstat )) then
  !  res = C_NULL_PTR
  !  call xtype_delete_c( self )
  !else
  !  call c_f_pointer( self%ptr, res, [self%len] )
  !end if
end function


!_PROC_EXPORT(xtype_char_f)
function xtype_char_f( self ) result(res)
  type(Xtype_t)           :: self
  character(len=self%len) :: res
end function

