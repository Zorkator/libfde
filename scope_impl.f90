
#include "adt/itfUtil.fpp"

module impl_scope__
  use adt_hashmap
  use adt_item
  use adt_ref
  use adt_string
  use adt_typeinfo
end module


!_PROC_EXPORT(scope_create_)
  function scope_create_() result(scope)
    use impl_scope__; implicit none
    type(HashMap_t), pointer :: scope
    allocate( scope )
    call initialize( scope )
  end function


!_PROC_EXPORT(scope_get_subscope_)
  function scope_get_subscope_( parent, id ) result(scope)
    use impl_scope__; implicit none
    type(HashMap_t)          :: parent
    character(len=*)         :: id
    type(HashMap_t), pointer :: scope
    type(Item_t),    pointer :: it

    if (getOrCreate( it, parent, id )) then
      allocate( scope );
      call initialize( scope )
      call assign( it, Item_of( ref_of( scope, bind = .true. ) ) )
    else
      if (dynamic_cast( scope, ref(it) )) continue;
    endif
  end function


!_PROC_EXPORT(scope_get_subscope_seq_)
  function scope_get_subscope_seq_( parent, id1, id2, id3, id4, id5, id6, id7, id8, id9 ) result(scope)
    use impl_scope__
    use adt_scope
    implicit none
    type(HashMap_t)            :: parent
    character(len=*)           :: id1, id2
    character(len=*), optional :: id3, id4, id5, id6, id7, id8, id9
    type(HashMap_t),   pointer :: scope

    scope => getScope( getScope( parent, id1 ), id2 )

# define __getSubScope(arg) \
    if (.not. present(arg)) return ;\
    scope => getScope( scope, arg )

    __getSubScope(id3)
    __getSubScope(id4)
    __getSubScope(id5)
    __getSubScope(id6)
    __getSubScope(id7)
    __getSubScope(id8)
    __getSubScope(id9)
  end function


!_PROC_EXPORT(print_scope)
  recursive &
  subroutine print_scope( scope, level )
    use impl_scope__; implicit none
    type(HashMap_t)           :: scope
    integer                   :: level
    character(len=2*level)    :: indent
    type(HashMapIndex_t)      :: idx
    type(HashMap_t),  pointer :: subscope
    type(Item_t),     pointer :: val
    type(Ref_t),      pointer :: valRef => null()
    type(TypeInfo_t), pointer :: ti
    character(len=255)        :: buffer

    indent = ' '
    idx    = index( scope )
    do while (is_valid(idx))
      val => value(idx)
      ti  => dynamic_type(val)

      if (is_ref(val)) then
        valRef => ref(val)
        ti     => dynamic_type(valRef)
      end if
      call write( buffer, val )
      print *, indent // str(key(idx)) // ' : ' // '[' // trim(adjustl(buffer)) // ']'
      if (associated(valRef)) then
        if (dynamic_cast( subscope, valRef )) then
          call print_scope( subscope, level + 1 )
        end if
      end if
      call next(idx)
    end do
  end subroutine


