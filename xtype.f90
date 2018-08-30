
module adt_xtype
  use iso_c_binding
  implicit none

# include "adt/type/xtype.ftype"

  interface Xtype
    module procedure create_by_xtype, create_by_chrstr
  end interface

contains

!_PROC_EXPORT(create_by_xtype)
  function create_by_xtype( other ) result(self)
    type(Xtype_t), optional :: other
    type(Xtype_t)           :: self
    call xtype_create_by_xtype_c( self, other )
  end function

!_PROC_EXPORT(create_by_chrstr)
  function create_by_chrstr( chrstr ) result(self)
    character(len=*) :: chrstr
    type(Xtype_t)    :: self
    call xtype_create_by_chrstr_c( self, chrstr )
  end function

end module

