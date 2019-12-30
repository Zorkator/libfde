
module test_plugin
  use fde_plugin
  use fde_convert
  use fde_stringref
  implicit none

  contains

  subroutine test_list_so()
    call plugin_iterate_so( printSO )
  end subroutine

  subroutine printSO( id, addr )
    type(StringRef_t) :: id
    integer           :: addr
    print*, hex(addr), ": ", str(id)
  end subroutine

end module

program plugintest
  use test_plugin 

  call test_list_so()
end program

