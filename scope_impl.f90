
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
  use adt_exception
end module


!_PROC_EXPORT(scope_create_)
  function scope_create_() result(scope)
    use impl_scope__; implicit none
    type(HashMap_t), pointer :: scope
    allocate( scope )
    call initialize( scope )
  end function


!_PROC_EXPORT(scope_get_processscope_c)
  subroutine scope_get_processscope_c( procScope )
    ! CAUTION: without the following use restriction gfortran
    !          screws up the c_loc assignment below!
    use adt_scope, only: getScope
    use iso_c_binding
    type(c_ptr), intent(inout) :: procScope
    procScope = c_loc( getScope() ) !< !!!
  end subroutine


!_PROC_EXPORT(scope_get_subscope_seq_)
  function scope_get_subscope_seq_( parent_, id1, id2, id3, id4, id5, id6, id7, id8, id9 ) result(scope)
    use impl_scope__
    use adt_scope
    implicit none
    type(HashMap_t)            :: parent_
    character(len=*)           :: id1
    character(len=*), optional :: id2, id3, id4, id5, id6, id7, id8, id9
    type(HashMap_t),   pointer :: scope

    scope => get_subscope_( parent_, id1 )

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
        call setParent( scope, parent_ )
      else
        if (dynamic_cast( scope, ref(it) )) continue;
      endif
    end function

  end function


!_PROC_EXPORT(scope_declare_callback_c)
  subroutine scope_declare_callback_c( self, ident )
    use impl_scope__
    implicit none
    type(HashMap_t)       :: self
    character(len=*)      :: ident
    type(Item_t), pointer :: cbItem
    procedure(),  pointer :: cbPtr
    
    cbItem => get( self, ident )
    cbPtr  => Callback_from_ref(ref(cbItem))
    if (.not. associated(cbPtr)) then
      cbItem = ref_from_Callback(cbPtr)
    end if
  end subroutine


!_PROC_EXPORT(scope_set_callback_c)
  integer &
  function scope_set_callback_c( self, ident, proc_ ) result(res)
    use impl_scope__
    implicit none
    type(HashMap_t)       :: self
    character(len=*)      :: ident
    procedure(), optional :: proc_
    procedure(), pointer  :: procPtr
    type(Item_t), pointer :: itemPtr

    procPtr => proc_
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


!_PROC_EXPORT(scope_get_item_)
  function scope_get_item_( scope, id, raise ) result(res)
    ! Get pointer to item <id> of given <scope>.
    ! The given id has to exist within scope or a KeyError is thrown
    ! The value referenced by the returned item can be accessed by
    !   either assigning it directly to a scalar variable
    !   (types have to match!) or by using a dynamic_cast to cast it
    !   into an appropreate value pointer.
    use impl_scope__
    implicit none
    type(HashMap_t)        :: scope
    character(len=*)       :: id
    logical,      optional :: raise
    type(Item_t), pointer  :: res
    logical                :: raise_

    if (present(raise)) then; raise_ = raise
                        else; raise_ = .true.
    end if
    res => getPtr( scope, id )
    if (.not. associated(res) .and. raise_) then
      call throw( KeyError, __no_item(id) )
    end if
  end function


!_PROC_EXPORT(scope_get_ref_)
  function scope_get_ref_( scope, id, raise ) result(res)
    ! Get a reference to variable <id> of given <scope>.
    ! The given id must exist within scope and it must refer to a
    !   variable reference.
    ! Otherwise, either a KeyError or a TypeError is thrown.
    use impl_scope__
    implicit none
    type(HashMap_t)        :: scope
    character(len=*)       :: id
    logical,      optional :: raise
    type(Ref_t),  pointer  :: res
    logical                :: raise_

    if (present(raise)) then; raise_ = raise
                        else; raise_ = .true.
    end if
    if (.not. dynamic_cast( res, getItem( scope, id, raise_  ) ) .and. raise_) then
      call throw( TypeError, __ill_var_ref(id) )
    end if
  end function


!_PROC_EXPORT(scope_get_procedure_)
  function scope_get_procedure_( scope, id, raise ) result(res)
    ! Get generic procedure pointer named <id> of given <scope>.
    ! A TypeError is thrown if the scope item is not a routine.
    use impl_scope__
    implicit none
    type(HashMap_t)        :: scope
    character(len=*)       :: id
    logical,      optional :: raise
    type(c_funptr)         :: res
    logical                :: raise_
    type(c_ptr),   pointer :: fptr

    if (present(raise)) then; raise_ = raise
                        else; raise_ = .true.
    end if
    if (.not. dynamic_cast( fptr, getItem( scope, id, raise_ )) .and. raise_) then
      call throw( TypeError, __type_mismatch_routine(id) )
    end if
    if (associated(fptr)) then; res = transfer( fptr, res )
                          else; res = c_null_funptr
    end if
  end function


!_PROC_EXPORT(scope_set_procedure_)
  subroutine scope_set_procedure_( scope, id, proc_ )
    use impl_scope__
    type(HashMap_t)  :: scope
    character(len=*) :: id
    procedure()      :: proc_
    type(c_ptr)      :: fptr
  
    fptr = transfer( c_funloc(proc_), fptr )
    call set( scope, id, Item_of(fptr) )
  end subroutine
