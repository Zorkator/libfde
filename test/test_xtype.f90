
module test_xtype
  use adt_xtype
  implicit none

  contains

  subroutine test_create()
    type(Xtype_t) :: xt, xt2

    xt  = Xtype()
    xt2 = Xtype(xt)
    xt2 = Xtype( "testinger text" )

    print *, storage_size(xt)/8

  end subroutine

end module

program dyn_cast
  use test_xtype 

  call test_create()
end program

