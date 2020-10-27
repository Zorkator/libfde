
module test_plugin
  use fde_plugin
  use fde_convert
  use fde_stringref
  implicit none

  contains

  subroutine test_list_so()
    integer           :: res
    ! dont print result of plugin_iterate_so() directly,
    ! as 'print' will end up in a deadlock when -pthread is used.
    res = plugin_iterate_so( printSO )
    print *, res
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

