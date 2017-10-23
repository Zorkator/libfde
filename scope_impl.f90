
#include "adt/itfUtil.fpp"
#include "adt/scope.fpp"

module impl_scope__
  use adt_hashmap
  use adt_scope
  use adt_item
  use adt_ref
  use adt_list
  use adt_string
  use adt_typeinfo
  use adt_basetypes
  use adt_exception
  use adt_containertypes

  contains

  function get_callbackList_( self, ident ) result(res)
    implicit none
    type(HashMap_t)       :: self
    character(len=*)      :: ident
    type(List_t), pointer :: res
    type(Ref_t),  pointer :: ref_

    res => null()
    if (dynamic_cast( ref_, getPtr( self, ident ) )) then
      if (dynamic_cast( res, ref_ )) continue
    end if
  end function
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
        if (.not. dynamic_cast( scope, ref(it) )) then
          call throw( TypeError, __type_mismatch_what("subscope", id_) )
        end if
      endif
    end function
  end function


!_PROC_EXPORT(scope_declare_callback_c)
  subroutine scope_declare_callback_c( self, ident )
    use impl_scope__
    implicit none
    type(HashMap_t)       :: self
    character(len=*)      :: ident
    type(List_t), pointer :: cbList
    type(Item_t), pointer :: it
    type(Ref_t),  pointer :: ref_

    if (getOrCreate( it, self, ident )) then
      allocate( cbList )
      call initialize( cbList )
      call assign( it, Item_of( ref_of( cbList, bind = .true. ) ) )
    else
      if (dynamic_cast( ref_, it )) then
        if (dynamic_cast( cbList, ref_ )) then
          call clear( cbList )
          return
        end if
      end if
      call throw( TypeError, __type_mismatch_what("event list", ident) )
    end if
  end subroutine


!_PROC_EXPORT(scope_connect_callback_c)
  integer &
  function scope_connect_callback_c( self, ident, proc_ ) result(res)
    use impl_scope__
    implicit none
    type(HashMap_t)        :: self
    character(len=*)       :: ident
    procedure(),  optional :: proc_
    procedure(),  pointer  :: procPtr
    type(List_t), pointer  :: cbList

    res    = hook_undeclared
    cbList => get_callbackList_( self, ident )
    if (associated( cbList )) then
      res     = hook_not_set
      procPtr => proc_
      if (associated( procPtr )) then
        call append( cbList, new_ListNode_of( ref_from_Callback(procPtr) ) )
        res = hook_set
      end if
    end if
  end function


!_PROC_EXPORT(scope_disconnect_callback_c)
  integer &
  function scope_disconnect_callback_c( self, ident, proc_ ) result(res)
    use impl_scope__
    implicit none
    type(HashMap_t)       :: self
    character(len=*)      :: ident
    procedure(), optional :: proc_
    procedure(), pointer  :: procPtr, cbf
    type(List_t), pointer :: cbList
    type(ListIndex_t)     :: idx, delIdx

    res    =  hook_undeclared
    cbList => get_callbackList_( self, ident )
    if (associated( cbList )) then
      procPtr => proc_
      if (associated( procPtr )) then
        idx = index( cbList )
        do while (is_valid(idx))
          cbf => Callback_from_ref( ref(idx) )
          if (associated( cbf, procPtr )) then
            delIdx = index( idx, 0 )
            call next( idx )
            call remove( delIdx )
          else
            call next( idx )
          end if
        end do
      else
        call clear( cbList )
      end if
      res = len( cbList )
    end if
  end function


!_PROC_EXPORT(scope_try_callback_c)
  integer &
  function scope_try_callback_c( self, ident, arg ) result(res)
    ! ident : ident string of hook to call
    ! res   : hook_undeclared | hook_not_set | hook_called
    use impl_scope__
    implicit none
    type(HashMap_t)           :: self
    character(len=*)          :: ident
    type(c_ptr),     optional :: arg
    type(List_t),    pointer  :: cbList
    procedure(),     pointer  :: cbf
    type(ListIndex_t)         :: idx

    res    = hook_undeclared
    cbList => get_callbackList_( self, ident )
    if (associated( cbList )) then
      if (len(cbList) > 0) then
        idx = index( cbList )
        do while (is_valid(idx))
          cbf => Callback_from_ref( ref(idx) )
          call cbf( arg )
          call next(idx)
        end do
        res = hook_called
      else
        res = hook_not_set
      end if
    end if
  end function


!_PROC_EXPORT(scope_invoke_callback_c)
  subroutine scope_invoke_callback_c( self, ident, arg )
    ! ident : ident string of hook to call
    use impl_scope__
    implicit none
    type(HashMap_t)       :: self
    character(len=*)      :: ident
    type(c_ptr), optional :: arg
    integer*4             :: res
    res = tryCallback( self, ident, arg )
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
