
#include "adt/itfUtil.fpp"

module adt_list__
  use adt_list
  use adt_ref
  use iso_c_binding
  implicit none

# define Node_t       Node_t__impl__
# define List_t       List_t__impl__
# define ListIndex_t  ListIndex_t__impl__

  type, public :: Node_t
    type(Node_t),             pointer :: prev => null(), next => null()
    type(TypeInfo_t), public, pointer :: typeInfo => null()
    type(void_t)                      :: padding
  end type

  type ValueNode_t
    type(Node_t) :: super
    integer      :: pseudoValue
  end type

  type, public :: List_t
    type(Node_t) :: node
    integer*4    :: length   =  0
  end type

  type, public :: ListIndex_t
    type (Node_t), public, pointer :: node   => null()
    type (List_t),         pointer :: host   => null()
    integer*4                      :: stride = 1
  end type

  type(List_t) :: al_stale_list

  interface
    subroutine NodeCloner( tgt, src )
      import Node_t
      type(Node_t), pointer, intent(out) :: tgt
      type(Node_t),           intent(in) :: src
    end subroutine

    recursive &
    subroutine al_clear( self )
      import List_t
      type(List_t), intent(inout) :: self
    end subroutine

    pure logical function ali_is_valid( self )
      import ListIndex_t
      type(ListIndex_t), intent(in) :: self
    end function

    pure logical function al_is_valid( self )
      import List_t
      type(List_t), intent(in) :: self
    end function
  
    subroutine al_initialize_list( self )
      import List_t
      type(List_t) :: self
    end subroutine

    subroutine al_initialize_node( self, nodeType )
      import Node_t, TypeInfo_t
      type (Node_t)              :: self
      type(TypeInfo_t), optional :: nodeType
    end subroutine

    subroutine al_link_node( node, prev, next )
      import Node_t
      type (Node_t) :: node, prev, next
    end subroutine

    subroutine al_unlink_node( prev, next )
      import Node_t
      type (Node_t) :: prev, next
    end subroutine

    subroutine al_insert_nodes( pos_hook, beg_prev, end_next )
      import Node_t
      type(Node_t) :: pos_hook, beg_prev, end_next
    end subroutine

    subroutine ali_insert_idx( self, idx )
      import ListIndex_t
      type(ListIndex_t) :: self, idx
    end subroutine

    subroutine ali_insert_idx_cnt( self, idx, cnt )
      import ListIndex_t
      type(ListIndex_t)      :: self, idx
      integer*4, intent(out) :: cnt
    end subroutine

    logical &
    function ali_advance_head( self, steps )
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
      integer*4                        :: steps
    end function

    logical &
    function ali_advance_foot( self, steps )
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
      integer*4                        :: steps, i
    end function

    function al_index_int( self, at, stride ) result(res)
      import List_t, ListIndex_t
      type(List_t),         intent(in) :: self
      integer*4,  optional, intent(in) :: at, stride
      type(ListIndex_t)                :: res
    end function

    function ali_pop_idx( self ) result(res)
      import ListIndex_t
      type(ListIndex_t) :: self, res
    end function

    function al_index_node( self, at, stride ) result(res)
      import List_t, Node_t, ListIndex_t
      type(List_t), target, intent(in) :: self
      type(Node_t),   target, optional :: at
      integer*4,              optional :: stride
      type(ListIndex_t)                :: res
    end function

    function ali_set_next( self ) result(res)
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
      logical                          :: res
    end function

    function ali_index_idx( self, stride ) result(res)
      import ListIndex_t
      type(ListIndex_t), intent(in) :: self
      integer*4,           optional :: stride
      type(ListIndex_t)             :: res
    end function
    
    subroutine al_append_node( self, node )
      import List_t, Node_t
      type (List_t), target :: self
      type (Node_t), target :: node
    end subroutine

    subroutine al_append_list( self, other )
      import List_t
      type (List_t), target :: self, other
    end subroutine

    subroutine ali_next( self )
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
    end subroutine
  end interface
    
end module


!_PROC_EXPORT(al_initialize_node)
  subroutine al_initialize_node( self, nodeType )
    use adt_list__, only: Node_t, TypeInfo_t
    implicit none
    type (Node_t),               target :: self
    type (TypeInfo_t), optional, target :: nodeType
    self%prev     => self
    self%next     => self
    self%typeInfo => nodeType
  end subroutine

  subroutine al_link_node( node, prev, next )
    use adt_list__, only: Node_t
    implicit none
    type (Node_t), target :: node, prev, next
    next%prev => node
    node%next => next
    node%prev => prev
    prev%next => node
  end subroutine

  subroutine al_unlink_node( prev, next )
    use adt_list__, only: Node_t
    implicit none
    type (Node_t), target :: prev, next
    prev%next => next
    next%prev => prev
  end subroutine

  subroutine al_remove_node( node )
    use adt_list__; implicit none
    type (Node_t), target :: node
    call al_unlink_node( node%prev, node%next )
    node%prev => null()
    node%next => null()
  end subroutine

  subroutine al_replace_node( old, new )
    use adt_list__; implicit none
    type (Node_t), target :: old, new
    call al_unlink_node( old%prev, old%next )
    call al_link_node( new, old%prev, old%next )
    old%prev => null()
    old%next => null()
  end subroutine

  subroutine al_insert_nodes( pos_hook, beg_prev, end_next )
    use adt_list__, only: Node_t, al_unlink_node
    implicit none
    type(Node_t), target :: pos_hook, beg_prev, end_next
    pos_hook % prev%next => beg_prev % next
    beg_prev % next%prev => pos_hook % prev
    pos_hook % prev      => end_next % prev
    end_next % prev%next => pos_hook
    call al_unlink_node( beg_prev, end_next )
  end subroutine


  subroutine al_initialize( self, has_proto, proto )
    use adt_list__; implicit none
    type(List_t), target :: self
    integer              :: has_proto
    type(List_t)         :: proto
    call al_initialize_list( self )
  end subroutine


  subroutine al_initialize_list( self )
    use adt_list__, only: List_t, al_initialize_node, al_is_valid, &
                          al_initialize_node, al_stale_list
    implicit none
    type(List_t),               target :: self
    call al_initialize_node( self%node )
    self%length = 0
    if (.not. al_is_valid( al_stale_list )) then
      call al_initialize_node( al_stale_list%node )
      al_stale_list%length = 0
    end if
  end subroutine

  
!_PROC_EXPORT(al_length)
  function al_length( self ) result(res)
    use adt_list__; implicit none
    type(List_t), target  :: self
    integer*4             :: res
    type(Node_t), pointer :: ptr

    res = self%length
    if (res < 0) then
      res =  0
      ptr => self%node%next
      do while (.not. associated( ptr, self%node ))
        res = res + 1
        ptr => ptr%next
      end do
      self%length = res
    end if
  end function


  pure logical function al_is_valid( self ) result(res)
    use adt_list__, only: List_t
    implicit none
    type(List_t), target, intent(in) :: self
    res = associated(self%node%prev) .and. associated(self%node%next)
  end function


  pure logical function al_is_empty( self ) result(res)
    use adt_list__; implicit none
    type (List_t), target, intent(in) :: self
    res = associated(self%node%prev, self%node) .and. associated(self%node%next, self%node)
  end function


  subroutine al_append_list( self, other )
    use adt_list__, only: List_t, al_insert_nodes
    implicit none
    type (List_t), target :: self, other
    call al_insert_nodes( self%node, other%node, other%node )
    self%length  = self%length + other%length
    other%length = 0
  end subroutine


  subroutine al_append_node( self, node )
    use adt_list__, only: List_t, Node_t, al_link_node
    implicit none
    type (List_t), target :: self
    type (Node_t), target :: node
    call al_link_node( node, self%node%prev, self%node )
    self%length = self%length + 1
  end subroutine


  subroutine al_append_idx( self, idx )
    use adt_list__; implicit none
    type (List_t), target :: self
    type (ListIndex_t)    :: idx
    call ali_insert_idx( al_index_int(self, tail, 0), idx )
  end subroutine


  subroutine al_delete( self )
    use adt_list__; implicit none
    type (List_t), target, intent(inout) :: self
    call al_clear( self )
    call al_clear( al_stale_list )
  end subroutine

  
!_PROC_EXPORT(al_clear)
  recursive &
  subroutine al_clear( self )
    use adt_list__, only: List_t, Node_t, ValueNode_t, &
                          al_initialize_list
    use iso_c_binding
    implicit none
    type(List_t), target, intent(inout) :: self
    type(Node_t),               pointer :: ptr, delPtr
    type(ValueNode_t),          pointer :: valNodePtr

    ptr => self%node%next
    do while (.not. associated( ptr, self%node ))
      delPtr => ptr
      ptr    => ptr%next
      if (associated( delPtr%typeInfo%subtype%deleteProc )) then
        call c_f_pointer( c_loc(delPtr), valNodePtr )
        call delPtr%typeInfo%subtype%deleteProc( valNodePtr%pseudoValue )
      end if
      deallocate( delPtr )
    end do
    call al_initialize_list( self )
  end subroutine


  function ali_dynamic_type( self ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(TypeInfo_t),     pointer :: res
    res => self%node%typeInfo
    if (.not. associated(res)) &
      res => type_void
  end function


  !function al_index( self, begin, end, stride ) result(res)
  !  type(List_t),        intent(in) :: self
  !  integer*4, optional, intent(in) :: begin, end, stride
  !  type(ListIndex_t)               :: res
  !  integer*4 :: a,b,c
  !  a = min( begin, end )
  !end function

  function al_index_node( self, at, stride ) result(res)
    use adt_list__, only: List_t, Node_t, ListIndex_t
    implicit none
    type(List_t), target, intent(in) :: self
    type(Node_t),   target, optional :: at
    integer*4,              optional :: stride
    type(ListIndex_t)                :: res
    logical                          :: ok
    res%host => self
    if (present(at))   then; res%node => at
                       else; res%node => self%node%next
    end if
    if (present(stride))     res%stride = stride
  end function


  function ali_index_idx( self, stride ) result(res)
    use adt_list__, only: ListIndex_t, al_index_node
    implicit none
    type(ListIndex_t), intent(in) :: self
    integer*4,           optional :: stride
    type(ListIndex_t)             :: res
    res = al_index_node( self%host, self%node, stride )
  end function


  function al_index_int( self, at, stride ) result(res)
    use adt_list__, only: List_t, ListIndex_t, ali_advance_foot
    implicit none
    type(List_t), target, intent(in) :: self
    integer*4,  optional, intent(in) :: at
    integer*4,  optional, intent(in) :: stride
    type(ListIndex_t)                :: res
    logical                          :: ok
    res%host => self
    res%node => self%node
    if (present(at)) then; ok = ali_advance_foot( res, at )
                     else; res%node => res%node%next
    end if
    if (present(stride))   res%stride = stride
  end function


  function ali_set_prev( self ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(inout) :: self
    logical                          :: res
    res = ali_advance_head( self, -self%stride )
  end function


  subroutine ali_prev( self )
    use adt_list__; implicit none
    type(ListIndex_t), intent(inout) :: self
    logical                          :: ok
    ok = ali_advance_head( self, -self%stride )
  end subroutine


  function ali_get_prev( self ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(ListIndex_t)             :: res
    logical                       :: ok
    res = self
    ok = ali_advance_head( res, -res%stride )
  end function


  function ali_set_next( self ) result(res)
    use adt_list__, only: ListIndex_t, ali_advance_head
    implicit none
    type(ListIndex_t), intent(inout) :: self
    logical                          :: res
    res = ali_advance_head( self, self%stride )
  end function


  subroutine ali_next( self )
    use adt_list__, only: ListIndex_t, ali_advance_head
    implicit none
    type(ListIndex_t), intent(inout) :: self
    logical                          :: ok
    ok = ali_advance_head( self, self%stride )
  end subroutine


  function ali_get_next( self ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(ListIndex_t)             :: res
    logical                       :: ok
    res = self
    ok = ali_advance_head( res, res%stride )
  end function


  logical &
  function ali_advance_foot( self, steps ) result(res)
    use adt_list__, only: ListIndex_t
    implicit none
    type(ListIndex_t), target, intent(inout) :: self
    integer*4                                :: steps, i

    res = .false.
    if (steps > 0) then
      do i = 1, steps
        self%node => self%node%next
        res = .not. associated(self%node, self%host%node)
        if (.not. res) return
      end do
    else
      do i = 1, -steps
        self%node => self%node%prev
        res = .not. associated(self%node, self%host%node)
        if (.not. res) return
      end do
    end if
  end function
  

  logical &
  function ali_advance_head( self, steps ) result(res)
    use adt_list__, only: ListIndex_t
    implicit none
    type(ListIndex_t), target, intent(inout) :: self
    integer*4                                :: steps, i

    res = .false.
    if (steps > 0) then
      do i = 1, steps
        res = .not. associated(self%node, self%host%node)
        if (.not. res) return
        self%node => self%node%next
      end do
    else
      do i = 1, -steps
        res = .not. associated(self%node, self%host%node)
        if (.not. res) return
        self%node => self%node%prev
      end do
    end if
  end function
  

  pure logical &
  function ali_is_valid( self ) result(res)
    use adt_list__, only: ListIndex_t
    implicit none
    type(ListIndex_t), target, intent(in) :: self
    res = associated(self%host) .and. associated(self%node)
    if (res) &
      res = .not. associated(self%node, self%host%node)
  end function

  
  pure logical &
  function ali_eq_ali( self, other ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self, other
    res = associated( self%node, other%node )
  end function

  
  pure logical &
  function ali_eq_node( self, node ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t),    intent(in) :: self
    type(Node_t), target, intent(in) :: node
    res = associated( self%node, node )
  end function

  
  pure logical &
  function ali_ne_ali( self, other ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self, other
    res = .not. associated( self%node, other%node )
  end function

  
  pure logical &
  function ali_ne_node( self, node ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t),    intent(in) :: self
    type(Node_t), target, intent(in) :: node
    res = .not. associated( self%node, node )
  end function

  
  subroutine ali_insert_list( self, lst )
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(List_t),          target :: lst
    call al_insert_nodes( self%node, lst%node, lst%node )
    self%host%length = self%host%length + lst%length
    lst%length       = 0
  end subroutine


  subroutine ali_insert_node( self, node )
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(Node_t),          target :: node
    call al_link_node( node, self%node%prev, self%node )
    self%host%length = self%host%length + 1
  end subroutine


  subroutine ali_insert_idx( self, idx )
    use adt_list__, only: ListIndex_t, ali_insert_idx_cnt
    implicit none
    type(ListIndex_t) :: self, idx
    integer*4         :: cnt
    call ali_insert_idx_cnt( self, idx, cnt )
  end subroutine


  subroutine ali_insert_idx_cnt( self, idx, cnt )
    use adt_list__, only: ListIndex_t, Node_t, ali_is_valid, ali_set_next, ali_next, al_insert_nodes
    implicit none
    type(ListIndex_t)      :: self, idx
    integer*4, intent(out) :: cnt
    type(Node_t),  pointer :: node
    logical                :: done

    cnt = 0
    do while (ali_is_valid( idx ))
      node => idx%node
      done = .not. ali_set_next(idx)
      cnt  = cnt + 1
      call al_insert_nodes( self%node, node%prev, node%next )
      self%host%length = self%host%length + 1
      idx%host%length  = idx%host%length - 1

      if (done) then; exit
                else; call ali_next(self)
      end if
    end do
  end subroutine


  subroutine ali_insert_range( self, beg, end )
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self, beg, end
    call al_insert_nodes( self%node, beg%node%prev, end%node%next )
    self%host%length = -1
    beg%host%length  = -1
  end subroutine


  subroutine ali_remove_idx( self )
    use adt_list__; implicit none
    type(ListIndex_t) :: self
    call ali_insert_idx( al_index_int(al_stale_list, tail, 0), self )
  end subroutine

  
!_PROC_EXPORT(al_pop_int)
  function al_pop_int( self, at ) result(res)
    use adt_list__; implicit none
    type(List_t), target :: self
    integer*4            :: at
    type(ListIndex_t)    :: res
    res = ali_index_idx( ali_pop_idx( al_index_int( self, at, 0 ) ), 0 )
  end function


!_PROC_EXPORT(ali_pop_idx)
  function ali_pop_idx( self ) result(res)
    use adt_list__, only: ListIndex_t, ali_insert_idx_cnt, al_index_int, al_stale_list, tail
    implicit none
    type(ListIndex_t) :: self, res
    integer*4         :: cnt
    call ali_insert_idx_cnt( al_index_int(al_stale_list, tail, 0), self, cnt )
    res = al_index_int( al_stale_list, -cnt )
  end function


!_PROC_EXPORT(al_assign_al)
  subroutine al_assign_al( lhs, rhs )
    use adt_list__, only: List_t, Node_t, NodeCloner, al_clear, al_append_node
    implicit none
    type(List_t), target, intent(inout) :: lhs
    type(List_t), target,    intent(in) :: rhs
    type(Node_t),               pointer :: copy, ptr, base
    procedure(NodeCloner),      pointer :: cloneNode

    base => rhs%node%next%prev !< sorry, but rhs is a shallow copy!
    if (.not. associated( base, lhs%node )) then
      call al_clear( lhs )
      ptr => base%next
      do while (.not. associated( ptr, base ))
        cloneNode => ptr%typeInfo%cloneObjProc
        call cloneNode( copy, ptr )
        call al_append_node( lhs, copy )
        ptr => ptr%next
      end do
    end if
  end subroutine


  subroutine al_assign_idx( lhs, rhs )
    use adt_list__; implicit none
    type(List_t), target, intent(inout) :: lhs
    type(ListIndex_t),       intent(in) :: rhs
    type(ListIndex_t)                   :: idx
    type(Node_t),               pointer :: copy
    procedure(NodeCloner),      pointer :: cloneNode
    type(List_t)                        :: tmp

    idx = rhs
    if (associated( idx%host, lhs )) then
      call al_initialize_list( tmp )
      call ali_insert_idx( al_index_int(tmp, tail, 0), idx )
      call al_clear( lhs )
      call al_append_list( lhs, tmp )
    else
      call al_clear( lhs )
      do while (ali_is_valid(idx))
        cloneNode => idx%node%typeInfo%cloneObjProc
        call cloneNode( copy, idx%node )
        call al_append_node( lhs, copy )
        call ali_next(idx)
      end do
    end if
  end subroutine

