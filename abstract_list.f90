
module abstract_list
  implicit none

  type, public :: Item
    private
    type (Item), pointer :: prev => null(), next => null()
  end type

  type, public :: List
    private
    type (Item)                  :: item
    procedure(), nopass, pointer :: clearItem => null()
  end type

  type, public :: ListIterator
    private
    type (Item), public, pointer :: node => null()
    type (List),         pointer :: host => null()
    integer                      :: step = 1
  end type

  contains

  subroutine al_initialize_item( self )
    type (Item), target :: self
    self%prev => self
    self%next => self
  end subroutine

  subroutine al_link_item( node, prev, next )
    type (Item), target :: node, prev, next
    next%prev => node
    node%next => next
    node%prev => prev
    prev%next => node
  end subroutine

  subroutine al_unlink_item( prev, next )
    type (Item), target :: prev, next
    prev%next => next
    next%prev => prev
  end subroutine

  subroutine al_remove_item( node )
    type (Item), target :: node
    call al_unlink_item( node%prev, node%next )
    node%prev => null()
    node%next => null()
  end subroutine

  subroutine al_replace_item( old, new )
    type (Item), target :: old, new
    call al_unlink_item( old%prev, old%next )
    call al_link_item( new, old%prev, old%next )
    old%prev => null()
    old%next => null()
  end subroutine



  subroutine al_initialize( self, clearProc )
    type (List),   target :: self
    procedure(), optional :: clearProc
    self%item%prev => self%item
    self%item%next => self%item
    self%clearItem => null()
    if (present(clearProc))  self%clearItem => clearProc
  end subroutine

  logical function al_is_valid( self ) result(res)
    type (List), target :: self
    res = associated(self%item%prev) .and. associated(self%item%next)
  end function

  logical function al_is_empty( self ) result(res)
    type (List), target :: self
    res = associated(self%item%prev, self%item) .and. associated(self%item%next, self%item)
  end function

  subroutine al_append_list( self, other )
    type (List), target :: self, other
    self%item  % prev%next => other%item % next
    other%item % next%prev => self%item  % prev
    self%item  % prev      => other%item % prev
    other%item % prev%next => self%item
    call al_initialize_item( other%item )
  end subroutine

  subroutine al_append_item( self, node )
    type (List), target :: self
    type (Item), target :: node
    call al_link_item( node, self%item%prev, self%item )
  end subroutine

  subroutine al_append_itr( self, itr )
    type (List), target :: self
    type (ListIterator) :: itr
  end subroutine



  subroutine al_delete( self )
    type (List), target, intent(inout) :: self
    type (Item),               pointer :: ptr, delPtr
    ptr => self%item%next
    do while (.not. associated( ptr, self%item ))
      delPtr => ptr
      ptr    => ptr%next
      if (associated( self%clearItem )) &
        call self%clearItem( delPtr )
      deallocate( delPtr )
    end do
    call al_initialize( self )
  end subroutine

end module



!##################################################################################################
#ifdef TEST

program testinger
  use abstract_list
  implicit none

  type :: MyItem
    type (Item) :: super
    integer*4   :: value
  end type

  type (List) :: l

  call al_initialize( l, myitem_delete )

  call al_delete( l )

  contains

  subroutine myitem_delete( node )
    use iso_c_binding
    type (Item),    target :: node
    type (MyItem), pointer :: ptr
    call c_f_pointer( c_loc(node), ptr )
    ptr%value = 0 ! call delete( ptr%value )
  end subroutine

  function myitem_downcast( node ) result(res)
    use iso_c_binding
    type (Item),    target :: node
    type (MyItem), pointer :: res
    call c_f_pointer( c_loc(node), res )
  end function

  !subroutine mylist_delete( self )
  !  type (MyList) :: self
  !  type (Item),   pointer :: ptr
  !  type (MyItem), pointer :: delPtr

  !  ptr => self%super%item%next
  !  do while (.not. associated( ptr, self%super%item ))
  !    call c_f_pointer( c_loc(ptr), delPtr )
  !    ptr => ptr%next
  !    delPtr%value = 0 !< call delete(delPtr%value)
  !    deallocate( delPtr )
  !  end do
  !end subroutine

end program

#endif

