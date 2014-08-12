
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
  interface delete    ; module procedure al_delete                        ; end interface
  interface item_type ; module procedure al_item_type                     ; end interface

  interface iterator  ; module procedure al_iterator_node, al_iterator_itr, al_iterator_int; end interface
  interface front     ; module procedure al_front                         ; end interface
  interface back      ; module procedure al_back                          ; end interface
  interface end       ; module procedure al_end                           ; end interface
  interface prev      ; module procedure ali_prev                         ; end interface
  interface next      ; module procedure ali_next                         ; end interface
  interface get_prev  ; module procedure ali_get_prev                     ; end interface
  interface get_next  ; module procedure ali_get_next                     ; end interface
  interface insert    ; module procedure ali_insert_item, ali_insert_list, ali_insert_range; end interface

  interface operator(==); module procedure ali_eq_ali, ali_eq_item        ; end interface
  interface operator(/=); module procedure ali_ne_ali, ali_ne_item        ; end interface

  integer*4, parameter :: first = 1, last = -1, tail = 0

  public :: initialize
  public :: len
  public :: is_valid
  public :: is_empty   
  public :: append
  public :: delete
  public :: clear

  public :: iterator
  public :: front, back, end
  public :: prev, get_prev
  public :: next, get_next
  public :: insert
  public :: first, last, tail

  public :: operator(==), operator(/=)


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

  subroutine al_insert_items( pos_hook, beg_prev, end_next )
    type(Item_t), target :: pos_hook, beg_prev, end_next
    pos_hook % prev%next => beg_prev % next
    beg_prev % next%prev => pos_hook % prev
    pos_hook % prev      => end_next % prev
    end_next % prev%next => pos_hook
    call al_unlink_item( beg_prev, end_next )
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

  
  function al_length( self ) result(res)
    type(List_t), target  :: self
    integer*4             :: res
    type(Item_t), pointer :: ptr

    res = self%length
    if (res < 0) then
      res =  0
      ptr => self%item%next
      do while (.not. associated( ptr, self%item ))
        res = res + 1
        ptr => ptr%next
      end do
      self%length = res
    end if
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
    call al_insert_items( self%item, other%item, other%item )
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


  function al_iterator_node( self, at, step ) result(res)
    type(List_t), target, intent(in) :: self
    type(Item_t),   target, optional :: at
    integer*4,              optional :: step
    type(ListIterator_t)             :: res
    logical                          :: ok
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
    logical                          :: ok
    res%host => self
    res%node => self%item
    if (present(step)) &
      res%step = step
    ok = ali_advance_foot( res, at )
  end function


  function al_front( self ) result(res)
    type(List_t), target, intent(in) :: self
    type(ListIterator_t)             :: res
    res%host => self
    res%node => self%item%next
  end function

    
  function al_back( self ) result(res)
    type(List_t), target, intent(in) :: self
    type(ListIterator_t)             :: res
    res%host => self
    res%node => self%item%prev
  end function


  function al_end( self ) result(res)
    type(List_t), target, intent(in) :: self
    type(ListIterator_t)             :: res
    res%host => self
    res%node => self%item
  end function


  subroutine ali_prev( self )
    type(ListIterator_t), intent(inout) :: self
    logical                             :: ok
    ok = ali_advance_head( self, -self%step )
  end subroutine


  function ali_get_prev( self ) result(res)
    type(ListIterator_t), intent(in) :: self
    type(ListIterator_t)             :: res
    logical                          :: ok
    res = self
    ok = ali_advance_head( res, -res%step )
  end function


  subroutine ali_next( self )
    type(ListIterator_t), intent(inout) :: self
    logical                             :: ok
    ok = ali_advance_head( self, self%step )
  end subroutine


  function ali_get_next( self ) result(res)
    type(ListIterator_t), intent(in) :: self
    type(ListIterator_t)             :: res
    logical                          :: ok
    res = self
    ok = ali_advance_head( res, res%step )
  end function


  logical function ali_advance_foot( self, steps ) result(res)
    type(ListIterator_t), target, intent(inout) :: self
    integer*4                                   :: steps, i

    if (steps > 0) then
      do i = 1, steps
        self%node => self%node%next
        if (associated(self%node, self%host%item)) &
          exit
      end do
    else
      do i = 1, -steps
        self%node => self%node%prev
        if (associated(self%node, self%host%item)) &
          exit
      end do
    end if
    res = (i > abs(steps))
  end function
  

  logical function ali_advance_head( self, steps ) result(res)
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

  
  pure logical function ali_eq_ali( self, other ) result(res)
    type(ListIterator_t), intent(in) :: self, other
    res = associated( self%node, other%node )
  end function

  
  pure logical function ali_eq_item( self, item ) result(res)
    type(ListIterator_t), intent(in) :: self
    type(Item_t), target, intent(in) :: item
    res = associated( self%node, item )
  end function

  
  pure logical function ali_ne_ali( self, other ) result(res)
    type(ListIterator_t), intent(in) :: self, other
    res = .not. associated( self%node, other%node )
  end function

  
  pure logical function ali_ne_item( self, item ) result(res)
    type(ListIterator_t), intent(in) :: self
    type(Item_t), target, intent(in) :: item
    res = .not. associated( self%node, item )
  end function

  
  subroutine ali_insert_item( self, node )
    type(ListIterator_t), intent(in) :: self
    type(Item_t),             target :: node
    call al_link_item( node, self%node%prev, self%node )
    self%host%length = self%host%length + 1
  end subroutine


  subroutine ali_insert_list( self, list )
    type(ListIterator_t), intent(in) :: self
    type(List_t),             target :: list
    call al_insert_items( self%node, list%item, list%item )
    self%host%length = self%host%length + list%length
    list%length      = 0
  end subroutine

  
  subroutine ali_insert_range( self, beg, end )
    type(ListIterator_t), intent(in) :: self, beg, end
    call al_insert_items( self%node, beg%node%prev, end%node%next )
    self%host%length = -1
    beg%host%length  = -1
  end subroutine

end module

