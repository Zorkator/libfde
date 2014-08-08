
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
    integer*4                 :: length   =  0
    type(TypeInfo_t), pointer :: typeInfo => null()
  end type


  type, public :: ListIterator_t
    private
    type (Item_t), public, pointer :: node => null()
    type (List_t),         pointer :: host => null()
    integer*4                      :: step = 1
  end type

  
  interface initialize; module procedure al_typed_init                    ; end interface
  interface len       ; module procedure al_length                        ; end interface
  interface is_valid  ; module procedure al_is_valid, ali_is_valid        ; end interface
  interface is_empty  ; module procedure al_is_empty                      ; end interface
  interface append    ; module procedure al_append_list, al_append_item   ; end interface
  interface clear     ; module procedure al_clear                         ; end interface
  interface front     ; module procedure al_front                         ; end interface
  interface back      ; module procedure al_back                          ; end interface
  interface cfront    ; module procedure al_cfront                        ; end interface
  interface cback     ; module procedure al_cback                         ; end interface
  interface delete    ; module procedure al_delete                        ; end interface
  interface item_type ; module procedure al_item_type                     ; end interface

  interface iterator  ; module procedure al_iterator_node, al_iterator_itr, al_iterator_int; end interface
  interface inserter  ; module procedure al_inserter                      ; end interface
  interface prev      ; module procedure ali_prev                         ; end interface
  interface next      ; module procedure ali_next                         ; end interface

  public :: initialize
  public :: len
  public :: is_valid
  public :: is_empty   
  public :: append
  public :: front
  public :: back
  public :: cfront
  public :: cback
  public :: delete
  public :: clear
  public :: prev
  public :: next

  public :: iterator
  public :: inserter

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


  subroutine al_raw_init( self, ignored )
    type(List_t), target :: self
    integer              :: ignored
    self%item%prev => self%item
    self%item%next => self%item
    self%length    =  0
    self%typeInfo  => null()
  end subroutine


  subroutine al_typed_init( self, item_type )
    type(List_t),               target :: self
    type(TypeInfo_t), optional, target :: item_type
    call al_raw_init( self, 0 )
    self%typeInfo => item_type
  end subroutine

  
  pure function al_length( self ) result(res)
    type(List_t), intent(in) :: self
    integer*4                :: res
    res = self%length
  end function


  pure logical function al_is_valid( self, item_type ) result(res)
    type(List_t),               target, intent(in) :: self
    type(TypeInfo_t), optional, target, intent(in) :: item_type
    res = associated(self%item%prev) .and. associated(self%item%next)
    if (present( item_type )) &
      res = res .and. associated(self%typeInfo, item_type)
  end function


  pure logical function al_is_empty( self ) result(res)
    type (List_t), target, intent(in) :: self
    res = associated(self%item%prev, self%item) .and. associated(self%item%next, self%item)
  end function


  subroutine al_append_list( self, other )
    type (List_t), target :: self, other
    self%item  % prev%next => other%item % next
    other%item % next%prev => self%item  % prev
    self%item  % prev      => other%item % prev
    other%item % prev%next => self%item
    call al_initialize_item( other%item )
    self%length  = self%length + other%length
    other%length = 0
  end subroutine


  subroutine al_append_item( self, node )
    type (List_t), target :: self
    type (Item_t), target :: node
    call al_link_item( node, self%item%prev, self%item )
    self%length = self%length + 1
  end subroutine


  subroutine al_append_itr( self, itr )
    type (List_t), target :: self
    type (ListIterator_t) :: itr
  end subroutine


  subroutine al_clear( self )
    type (List_t), target, intent(inout) :: self
    call al_delete( self, self%typeInfo )
  end subroutine

  
  subroutine al_delete( self, item_type )
    type(List_t), target, intent(inout) :: self
    type(TypeInfo_t),  optional, target :: item_type
    type(Item_t),               pointer :: ptr, delPtr
    procedure(),                pointer :: delItem => null()

    if (associated( self%typeInfo )) &
      delItem => self%typeInfo%deleteProc

    ptr => self%item%next
    do while (.not. associated( ptr, self%item ))
      delPtr => ptr
      ptr    => ptr%next
      if (associated( delItem )) &
        call delItem( delPtr )
      deallocate( delPtr )
    end do
    call al_typed_init( self, item_type )
  end subroutine


  function al_item_type( self ) result(res)
    type(List_t),  intent(in) :: self
    type(TypeInfo_t), pointer :: res
    if (associated( self%typeInfo )) then; res => self%typeInfo
                                     else; res => type_void
    end if
  end function


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


  function al_iterator_node( self, at, step ) result(res)
    type(List_t), target, intent(in) :: self
    type(Item_t),   target, optional :: at
    integer*4,              optional :: step
    type(ListIterator_t)             :: res
    res%host => self
    if (present(at))   then; res%node => at
                       else; res%node => self%item%next
    end if
    if (present(step)) then; res%step = step
                       else; res%step = 1
    end if
  end function


  function al_iterator_itr( self, at, step ) result(res)
    type(List_t), target, intent(in) :: self
    type(ListIterator_t), intent(in) :: at
    integer*4,              optional :: step
    type(ListIterator_t)             :: res
    res = al_iterator_node( self, at%node, step )
  end function


  function al_iterator_int( self, at, step ) result(res)
    type(List_t), target, intent(in) :: self
    integer*4,            intent(in) :: at
    integer*4,              optional :: step
    type(ListIterator_t)             :: res
    type (Item_t),           pointer :: start
    logical                          :: ok

    if (at < 0) then; start => back(self)
                else; start => front(self)
    end if
    res = al_iterator_node( self, start, step )
    ok  = ali_advance( res, at )
  end function


  function al_inserter( self, at ) result(res)
    type(List_t), target, intent(in) :: self
    type(Item_t),   target, optional :: at
    type(ListIterator_t)             :: res
    res%host => self
    res%step =  0
    if (present(at))   then; res%node => at
                       else; res%node => self%item
    end if
  end function


  function ali_prev( self ) result(res)
    type(ListIterator_t), intent(in) :: self
    type(ListIterator_t)             :: res
    logical                          :: ok
    res = self
    ok  = ali_advance( res, -res%step )
  end function


  function ali_next( self ) result(res)
    type(ListIterator_t), intent(in) :: self
    type(ListIterator_t)             :: res
    logical                          :: ok
    res = self
    ok  = ali_advance( res, res%step )
  end function


  logical function ali_advance( self, steps ) result(res)
    type(ListIterator_t), target, intent(inout) :: self
    integer*4                                   :: steps, i

    if (steps > 0) then
      do i = 1, steps
        if (associated(self%node, self%host%item)) then; exit
                                                   else; self%node => self%node%next
        end if
      end do
    else
      do i = 1, -steps
        if (associated(self%node, self%host%item)) then; exit
                                                   else; self%node => self%node%prev
        end if
      end do
    end if
    res = (i > abs(steps))
  end function
  

  pure logical function ali_is_valid( self ) result(res)
    type(ListIterator_t), target, intent(in) :: self
    res = associated(self%host) .and. &
          associated(self%node) .and. &
    .not. associated(self%node, self%host%item)
  end function
  
end module

