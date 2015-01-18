
#include "adt/itfUtil.fpp"

module sim_data
  implicit none

  integer :: cnt, ios
  real*8  :: t, dt, te
  real*4, dimension(:),   allocatable :: real4_array
  real*4, dimension(:,:), allocatable :: real4_matrix

end module


!--------------------
module sim_access
  use adt_hashmap
  use adt_item
  use adt_ref
  use adt_basetypes
  implicit none

  type(HashMap_t),          target :: stateMap_
  type(HashMap_t),          target :: hookMap_
  procedure(Callback_itf), pointer :: null_cb => null()

# define _add_stateSymbol( sym ) \
    call set( stateMap_, trim(adjustl(_str(sym))), Item_of(ref_of(sym)) ) ;\
    print *, "added symbol <" // trim(adjustl(_str(sym))) // ">"

# define _set_hookPointTo( hookId, cb ) \
    call set( hookMap_, hookId, Item_of(ref_from_Callback(cb)) )

# define _add_hookPoint( hookId ) \
    _set_hookPointTo( hookId, null_cb )

# define _ALLOCATE( sym, dim ) \
    allocate( sym dim );       \
    _add_stateSymbol( sym )

  contains

  subroutine init_access()
    use sim_data
    implicit none
    integer, save :: need_init = 1

    if (need_init /= 0) then
      call initialize( stateMap_ )
      call initialize( hookMap_ )

      _add_stateSymbol( t )
      _add_stateSymbol( dt )
      _add_stateSymbol( te )
      _add_stateSymbol( cnt )
      _add_stateSymbol( ios )

      _add_hookPoint( 'start' )
      _add_hookPoint( 'step' )
      _add_hookPoint( 'finish' )

      need_init = 0
    endif
  end subroutine

  subroutine invoke_callback( hookId )
    implicit none
    character(len=*)                 :: hookId
    type(Item_t),            pointer :: item
    procedure(Callback_itf), pointer :: cb
    integer ::array(10000)

    item => getPtr( hookMap_, hookId )
    if (associated(item)) then
      cb => Callback_from_ref(ref(item))
      if (associated(cb)) then
        call cb()
      end if
    end if
  end subroutine

end module

!_PROC_EXPORT(get_maps)  
subroutine get_maps( stateMap, hookMap )
  use sim_access
  use iso_c_binding
  implicit none
  type(c_ptr), intent(inout) :: stateMap, hookMap
  stateMap = c_loc(stateMap_)
  hookMap  = c_loc(hookMap_)
  call init_access()
end subroutine


!_PROC_EXPORT(set_callback)  
logical &
function set_callback( hookId, cb ) result(res)
  use sim_access
  implicit none
  character(len=*) :: hookId
  external         :: cb

  res = .false.
  if (hasKey( hookMap_, hookId )) then
    _set_hookPointTo( hookId, cb )
    res = .true.
  end if
end function


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

    logical &
    function set_callback( hookId, cb )
      character(len=*) :: hookId
      external :: cb
    end function

    subroutine init_simulator()
    end subroutine

    subroutine run_simulation()
    end subroutine

  end interface
end module
!--------------------


program simulator
  call init_simulator()
  call run_simulation()
end 


!_PROC_EXPORT(init_simulator)  
subroutine init_simulator()
  use sim_data
  use sim_access
  implicit none

  ! initialize
  t   = 0
  dt  = 0.1
  te  = 10.0
  cnt = 0

  _ALLOCATE( real4_array,  (10) );    real4_array  = 0;
  _ALLOCATE( real4_matrix, (10,20) ); real4_matrix = 1;

end subroutine

  
!_PROC_EXPORT(run_simulation)  
subroutine run_simulation()
  use sim_data
  use sim_access
  implicit none

  call invoke_callback('start')
  do while (t <= te)
    call invoke_callback('step')
    write(6,*,iostat=ios) "t: ", t 
    cnt = cnt + 1
    real4_array(mod(cnt, size(real4_array)) + 1) = cnt
    t = t + dt
  end do
  call invoke_callback('finish')

end subroutine

