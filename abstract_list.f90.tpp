
module abstract_list
  use type_info
  use generic_ref
  implicit none
  private

  type, public :: Item_t
    private
    type (Item_t), pointer :: prev => null(), next => null()
  end type


  type, public :: List_t
    private
    type(Item_t)              :: item
    type(TypeInfo_t), pointer :: typeInfo => null()
  end type


  type, public :: ListIterator
    private
    type (Item_t), public, pointer :: node => null()
    type (List_t),         pointer :: host => null()
    integer                        :: step = 1
  end type

  
  interface initialize; module procedure al_initialize                 ; end interface
  interface append    ; module procedure al_append_list, al_append_item; end interface
  interface front     ; module procedure al_front                      ; end interface
  interface back      ; module procedure al_back                       ; end interface
  interface cfront    ; module procedure al_cfront                     ; end interface
  interface cback     ; module procedure al_cback                      ; end interface
  interface delete    ; module procedure al_delete                     ; end interface

  public :: initialize
  public :: append
  public :: front
  public :: back
  public :: cfront
  public :: cback
  public :: delete

  !_TypeReference_declare( public, List, type(List_t), scalar, \
  !     initProc   = al_raw_init, \
  !     deleteProc = al_delete,   \
  !     assignProc = None,  \
  !     cloneProc  = None )


!-----------------
  contains
!-----------------

  !_TypeReference_implementAll()


  subroutine al_initialize_item( self )
    type (Item_t), target :: self
    self%prev => self
    self%next => self
  end subroutine

  subroutine al_link_item( node, prev, next )
    type (Item_t), target :: node, prev, next
    next%prev => node
    node%next => next
    node%prev => prev
    prev%next => node
  end subroutine

  subroutine al_unlink_item( prev, next )
    type (Item_t), target :: prev, next
    prev%next => next
    next%prev => prev
  end subroutine

  subroutine al_remove_item( node )
    type (Item_t), target :: node
    call al_unlink_item( node%prev, node%next )
    node%prev => null()
    node%next => null()
  end subroutine

  subroutine al_replace_item( old, new )
    type (Item_t), target :: old, new
    call al_unlink_item( old%prev, old%next )
    call al_link_item( new, old%prev, old%next )
    old%prev => null()
    old%next => null()
  end subroutine


  subroutine al_raw_init( self )
    type(List_t),     target :: self
    self%item%prev => self%item
    self%item%next => self%item
    self%typeInfo  => null()
  end subroutine


  subroutine al_initialize( self, item_typeInfo )
    type(List_t),                 target :: self
    type(TypeInfo_t), optional, target :: item_typeInfo
    self%item%prev => self%item
    self%item%next => self%item
    self%typeInfo  => null()
    if (present(item_typeInfo)) &
      self%typeInfo => item_typeInfo
  end subroutine


  logical function al_is_valid( self ) result(res)
    type (List_t), target :: self
    res = associated(self%item%prev) .and. associated(self%item%next)
  end function


  logical function al_is_empty( self ) result(res)
    type (List_t), target :: self
    res = associated(self%item%prev, self%item) .and. associated(self%item%next, self%item)
  end function


  subroutine al_append_list( self, other )
    type (List_t), target :: self, other
    self%item  % prev%next => other%item % next
    other%item % next%prev => self%item  % prev
    self%item  % prev      => other%item % prev
    other%item % prev%next => self%item
    call al_initialize_item( other%item )
  end subroutine


  subroutine al_append_item( self, node )
    type (List_t), target :: self
    type (Item_t), target :: node
    call al_link_item( node, self%item%prev, self%item )
  end subroutine


  subroutine al_append_itr( self, itr )
    type (List_t), target :: self
    type (ListIterator)   :: itr
  end subroutine


  subroutine al_delete( self )
    type (List_t), target, intent(inout) :: self
    type (Item_t),               pointer :: ptr, delPtr
    ptr => self%item%next
    do while (.not. associated( ptr, self%item ))
      delPtr => ptr
      ptr    => ptr%next
      if (associated( self%typeInfo ) ) then
        if (associated( self%typeInfo%deleteProc )) &
          call self%typeInfo%deleteProc( delPtr )
      end if
      deallocate( delPtr )
    end do
    call al_initialize( self )
  end subroutine


  function al_front( self ) result(res)
    type (List_t), target, intent(in) :: self
    type (Item_t),            pointer :: res
    if (associated( self%item%next, self%item )) then; res => null()
                                                 else; res => self%item%next
    end if
  end function

    
  function al_back( self ) result(res)
    type (List_t), target, intent(in) :: self
    type (Item_t),            pointer :: res
    if (associated( self%item%prev, self%item )) then; res => null()
                                                 else; res => self%item%prev
    end if
  end function


  function al_cfront( self ) result(res)
    use iso_c_binding
    type (List_t), target, intent(in) :: self
    type (c_ptr)                      :: res
    if (associated( self%item%next, self%item )) then; res = C_NULL_PTR
                                                 else; res = c_loc( self%item%next )
    end if
  end function

    
  function al_cback( self ) result(res)
    use iso_c_binding
    type (List_t), target, intent(in) :: self
    type (c_ptr)                      :: res
    if (associated( self%item%prev, self%item )) then; res = C_NULL_PTR
                                                 else; res = c_loc( self%item%prev )
    end if
  end function

end module

