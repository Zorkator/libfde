
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
