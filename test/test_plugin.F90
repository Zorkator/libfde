
module test_plugin
  use fde_plugin
  use fde_convert
  use fde_stringref
  implicit none

  contains

  subroutine test_list_so()
    print *, plugin_iterate_so( printSO )
  end subroutine

  function printSO( id, addr ) result(res)
    type(StringRef_t) :: id
    integer           :: addr
    integer           :: res
    print*, hex(addr), ": ", str(id)
    res = 0
  end function

end module

program plugintest
  use test_plugin 

  call test_list_so()
end program

