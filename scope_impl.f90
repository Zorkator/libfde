
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

  ! thanks to another gfortran (7.2.0) bug, we have to redefine Callback_itf here!
# define Callback_itf  Callback_itf_
  interface
    subroutine Callback_itf_( arg )
      import c_ptr
      type(c_ptr) :: arg
    end subroutine
  end interface

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


!_PROC_EXPORT(scope_connected_callbacks_c)
  integer &
  function scope_connected_callbacks_c( self, ident ) result(res)
    use impl_scope__
    type(HashMap_t),  intent(in) :: self
    character(len=*), intent(in) :: ident
    type(List_t),       pointer  :: cbList

    res    =  hook_undeclared
    cbList => get_callbackList_( self, ident )
    if (associated( cbList )) then
      res = len( cbList )
    end if
  end function


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
    type(HashMap_t)                  :: self
    character(len=*)                 :: ident
    type(c_ptr),            optional :: arg
    type(c_ptr)                      :: arg_
    type(List_t),            pointer :: cbList
    procedure(Callback_itf), pointer :: cbf
    type(ListIndex_t)                :: idx

    res    = hook_undeclared
    cbList => get_callbackList_( self, ident )
    if (associated( cbList )) then
      if (len(cbList) > 0) then
        if (present(arg)) then
          arg_ = arg
        end if

        idx = index( cbList )
        do while (is_valid(idx))
          ! redefinition of Callback_itf above forces us to cast the hard way ...
          call c_f_procpointer( c_funloc(Callback_from_ref( ref(idx) )), cbf )
          call cbf( arg_ )
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


!_PROC_EXPORT(scope_get_item_f)
  function scope_get_item_f( scope, id, raise ) result(res)
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
    logical                :: raise_
    type(Item_t), pointer  :: res

    _optArg( raise_, raise, .true. )
    res => getPtr( scope, id )
    if (.not. associated(res) .and. raise_) then
      call throw( KeyError, __no_item(id) )
    end if
  end function


!_PROC_EXPORT(scope_get_ref_f)
  function scope_get_ref_f( scope, id, raise ) result(res)
    ! Get a reference to variable <id> of given <scope>.
    ! The given id must exist within scope and it must refer to a
    !   variable reference.
    ! Otherwise, either a KeyError or a TypeError is thrown.
    use impl_scope__
    implicit none
    type(HashMap_t)        :: scope
    character(len=*)       :: id
    logical,      optional :: raise
    logical                :: raise_
    type(Ref_t),  pointer  :: res

    _optArg( raise_, raise, .true. )
    if (.not. dynamic_cast( res, getItem( scope, id, raise_  ) ) .and. raise_) then
      call throw( TypeError, __ill_var_ref(id) )
    end if
  end function


!_PROC_EXPORT(scope_get_char_ptr_f)
  function scope_get_char_ptr_f( scope, id, assumedLen, raise ) result(res)
    use impl_scope__
    implicit none
    type(HashMap_t)            :: scope
    character(len=*)           :: id
    integer*4,        optional :: assumedLen
    logical,          optional :: raise
    logical                    :: raise_
    character(len=:),  pointer :: res

    _optArg( raise_, raise, .true. )
    if (present(assumedLen)) then; call retrievePtr_( assumedLen )
                             else; call retrievePtr_( 255 )
    end if
  contains

    subroutine retrievePtr_( len_ )
      integer,          intent(in) :: len_
      character(len=len_), pointer :: str

      if (.not. dynamic_cast( str, getRef( scope, id, raise_ ) ) .and. raise_) then
        call throw( TypeError, __ill_var_ref(id) )
      end if
      res => mapPtr_( str ) !< there are STILL problems with ifort's bounds remapping - so we do it ourselvs.
     end subroutine

     function mapPtr_( in ) result(out)
       character(len=*),             target :: in
       character(len=len_trim(in)), pointer :: out
       out => in
     end function
  end function


!_PROC_EXPORT(scope_set_procedure_c)
  subroutine scope_set_procedure_c( scope, id, proc_ )
    use impl_scope__
    type(HashMap_t)  :: scope
    character(len=*) :: id
    procedure()      :: proc_
    type(c_ptr)      :: fptr
  
    fptr = transfer( c_funloc(proc_), fptr )
    call set( scope, id, Item_of(fptr) )
  end subroutine


!_PROC_EXPORT(scope_get_procedure_c)
  subroutine scope_get_procedure_c( res, scope, id, raise )
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

    _optArg( raise_, raise, .true. )
    if (.not. dynamic_cast( fptr, getItem( scope, id, raise_ )) .and. raise_) then
      call throw( TypeError, __type_mismatch_routine(id) )
    end if
    if (associated(fptr)) then; res = transfer( fptr, res )
                          else; res = c_null_funptr
    end if
  end subroutine


!_PROC_EXPORT(scope_get_procedure_f)
  function scope_get_procedure_f( scope, id, raise ) result(res)
    ! Get generic procedure pointer named <id> of given <scope>.
    ! A TypeError is thrown if the scope item is not a routine.
    use impl_scope__
    implicit none
    type(HashMap_t)        :: scope
    character(len=*)       :: id
    logical,      optional :: raise
    type(c_funptr)         :: res
    call scope_get_procedure_c( res, scope, id, raise )
  end function

