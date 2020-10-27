
#include "fde/itfUtil.fpp"
#include "fde/scope.fpp"

!--------------------
module sim_reflection
  use fde_hashmap
  use fde_scope
  use fde_item
  use fde_ref
  use fde_string
  use fde_basetypes
  use fde_streamvisitor
  implicit none

  type(HashMap_t), pointer :: root_scope => null()
  type(HashMap_t), pointer :: hook_scope => null()
# undef _callHook
# define _callHook(id) \
    call invokeCallback( hook_scope, id )
# undef _ALLOCATE
# define _ALLOCATE( scope, sym, dim )  \
    allocate( sym dim )               ;\
    _set_scopeSymbol( scope, sym )

  contains

  subroutine init_reflection()
    type(StreamVisitor_t) :: streamer

    if (.not. associated(root_scope)) then
      root_scope => getScope('test_simulator')
      hook_scope => getScope( root_scope, 'hooks' )

      call declareCallback( hook_scope, 'start' )
      call declareCallback( hook_scope, 'step' )
      call declareCallback( hook_scope, 'finish' )

      streamer = StreamVisitor(0)
      call accept( getScope(), streamer%super )
    endif
  end subroutine
end module


module sim_data
  use sim_reflection
  implicit none

  integer                                     :: cnt, ios
  real*8                                      :: t, dt, te
  real*4,         dimension(:),   allocatable :: real4_array
  real*4,         dimension(:,:), allocatable :: real4_matrix
  character(10),  dimension(:),   allocatable :: id_array
  character(10),  dimension(:,:), allocatable :: id_table
  character(10),  dimension(:),   allocatable :: name_array

  contains

  subroutine init_sim_data()
    integer                  :: i
    type(HashMap_t), pointer :: scope

    scope => getScope( root_scope, 'sim_data')
    _set_scopeSymbol( scope, t )
    _set_scopeSymbol( scope, dt )
    _set_scopeSymbol( scope, te )
    _set_scopeSymbol( scope, cnt )
    _set_scopeSymbol( scope, ios )

    ! initialize
    t   = 0
    dt  = 0.1
    te  = 10.0
    cnt = 0
    _ALLOCATE( scope, real4_array,  (10) );    real4_array  = 0;
    _ALLOCATE( scope, real4_matrix, (10,20) ); real4_matrix = 1;
    _ALLOCATE( scope, id_array,     (10) );    id_array     = ' '
    _ALLOCATE( scope, id_table,     (6,7) );   id_table     = ' '
    _ALLOCATE( scope, name_array,   (20) );    name_array   = ' ' !<< FIXME: id_array and name_array share the same typeinfo, whose byteSize
                                                                  !           is set to the size of the first ref'ed table ...
    do i = 1,size(id_array)
      write( id_array(i), '(i10)' ) i
    end do

    do i = 1,size(name_array)
      write( name_array(i), '(A4I5)' ) 'name', i
    end do
  end subroutine
end module


!_PROC_EXPORT(run_c)
subroutine run_c()
  use sim_data
  use sim_reflection
  implicit none

  call init_reflection()
  call init_sim_data()

  _callHook('start')

  ! main loop
  do while (t <= te)
    _callHook('step')
    write(6,*,iostat=ios) "t: ", t
    cnt = cnt + 1
    real4_array(mod(cnt, size(real4_array)) + 1) = cnt
    t = t + dt
  end do

  _callHook('finish')
end subroutine


program simulator
  call run_c()
end


!_PROC_EXPORT(initialize_c)
subroutine initialize_c()
  use sim_reflection
  call init_reflection()
end subroutine


