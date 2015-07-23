
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

  function getItem_( map, ident, create ) result(res)
    type (HashMap_t),  intent(in) :: map
    character(len=*),  intent(in) :: ident
    logical, optional, intent(in) :: create
    type (Item_t), pointer        :: res
    logical                       :: create_missing

    if (present(create)) then; create_missing = create
    else                     ; create_missing = .false.
    end if

    if (create_missing) then; res => get( map, ident )
    else                    ; res => getPtr( map, ident )
    end if
  end function


  function getValRef_( map, ident, create ) result(res)
    type (HashMap_t),  intent(in) :: map
    character(len=*),  intent(in) :: ident
    logical, optional, intent(in) :: create
    type (Ref_t),         pointer :: res
    type (Item_t),        pointer :: item_ptr
   
    res      => null()
    item_ptr => getItem_( map, ident, create )
    if (associated(item_ptr)) then
      if (is_ref(item_ptr)) &
        res => ref(item_ptr)
    end if
  end function


# define _getValRef( ident, typeId ) \
    typeId( getValRef_( state, ident ) )

# define _getVal( ident ) \
    getItem_( state, ident )


  subroutine callback()
    real*8, pointer :: t
    real*8       :: t_val

    t => _getValRef( 't', real8 )
    t_val = _getVal( 't' )
    print *, 'callback @ t =', t
  end subroutine

end
