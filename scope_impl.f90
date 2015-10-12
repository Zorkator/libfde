
#include "adt/itfUtil.fpp"
#include "adt/scope.fpp"

module impl_scope__
  use adt_hashmap
  use adt_scope
  use adt_item
  use adt_ref
  use adt_string
  use adt_typeinfo
  use adt_basetypes
end module


!_PROC_EXPORT(scope_create_)
  function scope_create_() result(scope)
    use impl_scope__; implicit none
    type(HashMap_t), pointer :: scope
    allocate( scope )
    call initialize( scope )
  end function


!_PROC_EXPORT(scope_get_rootscope_c)
  subroutine scope_get_rootscope_c( procScope, ident )
    use impl_scope__
    use iso_c_binding
    type(c_ptr), intent(inout) :: procScope
    character(len=*)           :: ident
    procScope = c_loc( getScope( ident ) )
  end subroutine


!_PROC_EXPORT(scope_get_subscope_seq_)
  function scope_get_subscope_seq_( parent, id1, id2, id3, id4, id5, id6, id7, id8, id9 ) result(scope)
    use impl_scope__
    use adt_scope
    implicit none
    type(HashMap_t)            :: parent
    character(len=*)           :: id1
    character(len=*), optional :: id2, id3, id4, id5, id6, id7, id8, id9
    type(HashMap_t),   pointer :: scope

    scope => get_subscope_( parent, id1 )

# define __getSubScope(arg) \
    if (.not. present(arg)) return ;\
    scope => get_subscope_( scope, arg )

    __getSubScope(id2)
    __getSubScope(id3)
    __getSubScope(id4)
    __getSubScope(id5)
    __getSubScope(id6)
    __getSubScope(id7)
    __getSubScope(id8)
    __getSubScope(id9)

    contains

    function get_subscope_( parent_, id_ ) result(scope)
      type(HashMap_t)          :: parent_
      character(len=*)         :: id_
      type(HashMap_t), pointer :: scope
      type(Item_t),    pointer :: it

      if (getOrCreate( it, parent_, id_ )) then
        allocate( scope );
        call initialize( scope )
        call assign( it, Item_of( ref_of( scope, bind = .true. ) ) )
      else
        if (dynamic_cast( scope, ref(it) )) continue;
      endif
    end function

  end function


!_PROC_EXPORT(scope_declare_callback_c)
  subroutine scope_declare_callback_c( self, ident )
    use impl_scope__
    implicit none
    type(HashMap_t)      :: self
    character(len=*)     :: ident
    procedure(), pointer :: null_callback
    null_callback => null()
    call set( self, ident, Item_of(ref_from_Callback(null_callback)) )
  end subroutine


!_PROC_EXPORT(scope_set_callback_c)
  integer &
  function scope_set_callback_c( self, ident, proc ) result(res)
    use impl_scope__
    implicit none
    type(HashMap_t)       :: self
    character(len=*)      :: ident
    procedure(), optional :: proc
    procedure(), pointer  :: procPtr
    type(Item_t), pointer :: itemPtr

    procPtr => proc
    res = 0
    if (hasKey( self, ident ) .or. .not. associated(procPtr)) then
      call set( self, ident, Item_of(ref_from_Callback(procPtr)) )
      res = 1
    end if
  end function


!_PROC_EXPORT(scope_try_callback_c)
  integer &
  function scope_try_callback_c( self, ident, argScope ) result(code)
    ! ident : ident string of hook to call
    ! code  : hook_undeclared | hook_not_set | hook_called
    use impl_scope__
    implicit none
    type(HashMap_t)           :: self
    character(len=*)          :: ident
    type(HashMap_t), optional :: argScope
    procedure(),      pointer :: cb
    type(Item_t),     pointer :: hookItem

    code     = hook_undeclared
    hookItem => getPtr( self, ident )
    if (associated( hookItem )) then
      code = hook_not_set
      cb   => Callback_from_ref(ref(hookItem))
      if (associated( cb )) then
        if (present(argScope)) &
          call set( argScope, '__hook__', Item_of(ident) )
        call cb()
        if (present(argScope)) &
          call remove( argScope, '__hook__' )
        code = hook_called
      end if
    end if
  end function


!_PROC_EXPORT(scope_invoke_callback_c)
  subroutine scope_invoke_callback_c( self, ident, argScope )
    ! ident : ident string of hook to call
    use impl_scope__
    implicit none
    type(HashMap_t)           :: self
    character(len=*)          :: ident
    type(HashMap_t), optional :: argScope
    integer*4                 :: code
    code = tryCallback( self, ident, argScope )
  end subroutine


