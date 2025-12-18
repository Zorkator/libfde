
#include "fde/itfUtil.fpp"
#include "fde/string.fpp"

# define __rootLocator__   _this_file_basename()
# include "fde/scope.fpp"

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

  type(StreamVisitor_t)    :: fout
  type(HashMap_t), pointer :: root_scope => null()
  type(HashMap_t), pointer :: hook_scope => null()
  integer                  :: istat

contains

  subroutine init_reflection()
    if (.not. associated(root_scope)) then
      root_scope => __rootScope__
      hook_scope => __hookScope__
      call declareCallback( hook_scope, 'start' )
      call declareCallback( hook_scope, 'step' )
      call declareCallback( hook_scope, 'finish' )
      fout = StreamVisitor(0)
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
    _REALLOCATE_visible( scope, real4_array,  (5) );   real4_array  = 0;
    _REALLOCATE_visible( scope, real4_matrix, (5,3) ); real4_matrix = 1;
    _REALLOCATE_visible( scope, id_array,     (10) );  id_array     = ' '
    _REALLOCATE_visible( scope, id_table,     (3,5) ); id_table     = ' '
    _REALLOCATE_visible( scope, name_array,   (10) );  name_array   = ' ' !<< FIXME: id_array and name_array share the same typeinfo, whose byteSize
                                                                          !           is set to the size of the first ref'ed table ...
    do i = 1,size(id_array)
      write( id_array(i), '(i10)' ) i
    end do

    do i = 1,size(name_array)
      write( name_array(i), '(A4,I5)' ) 'name', i
    end do

    ! print process scope ...
    call accept( getScope(), fout%super )
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
      ! perform timestep ...
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


