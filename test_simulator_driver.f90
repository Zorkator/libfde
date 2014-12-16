
module sim_itf
  use adt_hashmap
  use adt_item
  use adt_ref
  use adt_basetypes
  use iso_c_binding
  implicit none

  interface
    subroutine get_maps( state_ptr, hooks_ptr )
      import c_ptr
      type(c_ptr), intent(inout) :: state_ptr, hooks_ptr
    end subroutine

    subroutine init_simulator()
    end subroutine

    subroutine run_simulation()
    end subroutine

    logical &
    function set_callback( hookId, cb )
      character(len=*) :: hookId
      external :: cb
    end function
  end interface

end module

program sim_driver
  use sim_itf
  implicit none

  type(c_ptr)              :: state_ptr, hooks_ptr
  type(HashMap_t), pointer :: state, hooks
  type(Item_t),    pointer :: item_ref
  real*8,          pointer :: real_ref

  call get_maps( state_ptr, hooks_ptr )
  call c_f_pointer( state_ptr, state )
  call c_f_pointer( hooks_ptr, hooks )
  call init_simulator()

  print *, set_callback( 'step', callback )

  print *, len(state)
  print *, len(hooks)

  item_ref => get( state, 't' )
  real_ref => real8(ref(get( state, 't' )))

  print *, real_ref

  call run_simulation()

  contains

  subroutine callback()
    print *, 'callback @ t =', real8(ref(get( state, 't' )))
  end subroutine

end
