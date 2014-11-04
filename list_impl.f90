
#include "adt/itfUtil.fpp"

module adt_list__
  use adt_list
  use adt_ref
  use iso_c_binding
  implicit none

# define ListNode_t   Node_t__impl__
# define List_t       List_t__impl__
# define ListIndex_t  ListIndex_t__impl__

  type, public :: ListNode_t
    type(ListNode_t),         pointer :: prev => null(), next => null()
    type(TypeInfo_t), public, pointer :: typeInfo => null()
    type(void_t)                      :: padding
  end type

  type ValueNode_t
    type(ListNode_t) :: super
    integer      :: pseudoValue
  end type

  type, public :: List_t
    type(ListNode_t) :: node
    integer(kind=4)  :: length   =  0
  end type

  type, public :: ListIndex_t
    type (ListNode_t), public, pointer :: node   => null()
    type (List_t),             pointer :: host   => null()
    integer(kind=4)                    :: stride = 1
  end type

  type(List_t) :: list_stale_list

  interface
    subroutine NodeCloner( tgt, src )
      import ListNode_t
      type(ListNode_t), pointer, intent(out) :: tgt
      type(ListNode_t),           intent(in) :: src
    end subroutine

    recursive &
    subroutine list_clear( self )
      import List_t
      type(List_t), intent(inout) :: self
    end subroutine

    pure logical function listindex_is_valid( self )
      import ListIndex_t
      type(ListIndex_t), intent(in) :: self
    end function

    pure logical function list_is_valid( self )
      import List_t
      type(List_t), intent(in) :: self
    end function
  
    subroutine list_init( self )
      import List_t
      type(List_t) :: self
    end subroutine

    subroutine listnode_init( self, nodeType )
      import ListNode_t, TypeInfo_t
      type (ListNode_t)              :: self
      type(TypeInfo_t), optional :: nodeType
    end subroutine

    subroutine list_link_node( node, prev, next )
      import ListNode_t
      type (ListNode_t) :: node, prev, next
    end subroutine

    subroutine list_unlink_node( prev, next )
      import ListNode_t
      type (ListNode_t) :: prev, next
    end subroutine

    subroutine list_insert_nodes( pos_hook, beg_prev, end_next )
      import ListNode_t
      type(ListNode_t) :: pos_hook, beg_prev, end_next
    end subroutine

    subroutine listindex_insert_idx( self, idx )
      import ListIndex_t
      type(ListIndex_t) :: self, idx
    end subroutine

    subroutine listindex_insert_idx_cnt( self, idx, cnt )
      import ListIndex_t
      type(ListIndex_t)            :: self, idx
      integer(kind=4), intent(out) :: cnt
    end subroutine

    logical &
    function listindex_advance_head( self, steps )
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
      integer(kind=4)                  :: steps
    end function

    logical &
    function listindex_advance_foot( self, steps )
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
      integer(kind=4)                  :: steps, i
    end function

    function list_index_int( self, at, stride ) result(res)
      import List_t, ListIndex_t
      type(List_t),              intent(in) :: self
      integer(kind=4), optional, intent(in) :: at, stride
      type(ListIndex_t)                     :: res
    end function

    function listindex_pop_idx( self ) result(res)
      import ListIndex_t
      type(ListIndex_t) :: self, res
    end function

    function list_index_node( self, at, stride ) result(res)
      import List_t, ListNode_t, ListIndex_t
      type(List_t),   target, intent(in) :: self
      type(ListNode_t), target, optional :: at
      integer(kind=4),          optional :: stride
      type(ListIndex_t)                  :: res
    end function

    function listindex_set_next( self ) result(res)
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
      logical                          :: res
    end function

    function listindex_index_idx( self, stride ) result(res)
      import ListIndex_t
      type(ListIndex_t), intent(in) :: self
      integer(kind=4),     optional :: stride
      type(ListIndex_t)             :: res
    end function
    
    subroutine list_append_node( self, node )
      import List_t, ListNode_t
      type (List_t), target :: self
      type (ListNode_t), target :: node
    end subroutine

    subroutine list_append_list( self, other )
      import List_t
      type (List_t), target :: self, other
    end subroutine

    subroutine listindex_next( self )
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
    end subroutine

    subroutine list_assign_list( lhs, rhs )
      import List_t
      type(List_t), intent(inout) :: lhs
      type(List_t),    intent(in) :: rhs
    end subroutine
  end interface
    
end module


!_PROC_EXPORT(list_object_size)
  integer(kind=4) &
  function list_object_size() result(res)
    use adt_list__; implicit none
    type (List_t) :: tmp
    res = storage_size(tmp) / 8
  end function


!_PROC_EXPORT(listindex_object_size)
  integer(kind=4) &
  function listindex_object_size() result(res)
    use adt_list__; implicit none
    type (ListIndex_t) :: tmp
    res = storage_size(tmp) / 8
  end function


  subroutine listnode_init( self, nodeType )
    use adt_list__, only: ListNode_t, TypeInfo_t
    implicit none
    type (ListNode_t),               target :: self
    type (TypeInfo_t), optional, target :: nodeType
    self%prev     => self
    self%next     => self
    self%typeInfo => nodeType
  end subroutine

  subroutine list_link_node( node, prev, next )
    use adt_list__, only: ListNode_t
    implicit none
    type (ListNode_t), target :: node, prev, next
    next%prev => node
    node%next => next
    node%prev => prev
    prev%next => node
  end subroutine

  subroutine list_unlink_node( prev, next )
    use adt_list__, only: ListNode_t
    implicit none
    type (ListNode_t), target :: prev, next
    prev%next => next
    next%prev => prev
  end subroutine

  subroutine list_remove_node( node )
    use adt_list__; implicit none
    type (ListNode_t), target :: node
    call list_unlink_node( node%prev, node%next )
    node%prev => null()
    node%next => null()
  end subroutine

  subroutine list_replace_node( old, new )
    use adt_list__; implicit none
    type (ListNode_t), target :: old, new
    call list_unlink_node( old%prev, old%next )
    call list_link_node( new, old%prev, old%next )
    old%prev => null()
    old%next => null()
  end subroutine

  subroutine list_insert_nodes( pos_hook, beg_prev, end_next )
    use adt_list__, only: ListNode_t, list_unlink_node
    implicit none
    type(ListNode_t), target :: pos_hook, beg_prev, end_next
    pos_hook % prev%next => beg_prev % next
    beg_prev % next%prev => pos_hook % prev
    pos_hook % prev      => end_next % prev
    end_next % prev%next => pos_hook
    call list_unlink_node( beg_prev, end_next )
  end subroutine


!_PROC_EXPORT(list_init_by_proto)
  subroutine list_init_by_proto( self, has_proto, proto )
    use adt_list__; implicit none
    type(List_t), target :: self
    integer              :: has_proto
    type(List_t)         :: proto
    call list_init( self )
    if (has_proto /= 0) &
      call list_assign_list( self, proto )
  end subroutine


!_PROC_EXPORT(list_init)
  subroutine list_init( self )
    use adt_list__, only: List_t, listnode_init, list_is_valid, &
                          listnode_init, list_stale_list
    implicit none
    type(List_t),               target :: self
    call listnode_init( self%node )
    self%length = 0
    if (.not. list_is_valid( list_stale_list )) then
      call listnode_init( list_stale_list%node )
      list_stale_list%length = 0
    end if
  end subroutine

  
!_PROC_EXPORT(list_length)
  function list_length( self ) result(res)
    use adt_list__; implicit none
    type(List_t),     target  :: self
    integer(kind=4)           :: res
    type(ListNode_t), pointer :: ptr

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


!_PROC_EXPORT(list_is_valid)
  pure logical &
  function list_is_valid( self ) result(res)
    use adt_list__, only: List_t
    implicit none
    type(List_t), target, intent(in) :: self
    res = associated(self%node%prev) .and. associated(self%node%next)
  end function


!_PROC_EXPORT(list_is_empty)
  pure logical &
  function list_is_empty( self ) result(res)
    use adt_list__; implicit none
    type (List_t), target, intent(in) :: self
    res = associated(self%node%prev, self%node) .and. associated(self%node%next, self%node)
  end function


!_PROC_EXPORT(list_append_list)
  subroutine list_append_list( self, other )
    use adt_list__, only: List_t, list_insert_nodes
    implicit none
    type (List_t), target :: self, other
    call list_insert_nodes( self%node, other%node, other%node )
    self%length  = self%length + other%length
    other%length = 0
  end subroutine


!_PROC_EXPORT(list_append_node)
  subroutine list_append_node( self, node )
    use adt_list__, only: List_t, ListNode_t, list_link_node
    implicit none
    type (List_t), target :: self
    type (ListNode_t), target :: node
    call list_link_node( node, self%node%prev, self%node )
    self%length = self%length + 1
  end subroutine


!_PROC_EXPORT(list_append_idx)
  subroutine list_append_idx( self, idx )
    use adt_list__; implicit none
    type (List_t), target :: self
    type (ListIndex_t)    :: idx
    call listindex_insert_idx( list_index_int(self, tail, 0), idx )
  end subroutine


!_PROC_EXPORT(list_delete)
  subroutine list_delete( self )
    use adt_list__; implicit none
    type (List_t), target, intent(inout) :: self
    call list_clear( self )
    call list_clear( list_stale_list )
  end subroutine

  
!_PROC_EXPORT(list_clear)
  recursive &
  subroutine list_clear( self )
    use adt_list__, only: List_t, ListNode_t, ValueNode_t, &
                          list_init
    use iso_c_binding
    implicit none
    type(List_t), target, intent(inout) :: self
    type(ListNode_t),               pointer :: ptr, delPtr
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
    call list_init( self )
  end subroutine


!_PROC_EXPORT(listindex_dynamic_type)
  function listindex_dynamic_type( self ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(TypeInfo_t),     pointer :: res
    res => self%node%typeInfo
    if (.not. associated(res)) &
      res => type_void
  end function


  !function list_index( self, begin, end, stride ) result(res)
  !  type(List_t),              intent(in) :: self
  !  integer(kind=4), optional, intent(in) :: begin, end, stride
  !  type(ListIndex_t)                     :: res
  !  integer(kind=4) :: a,b,c
  !  a = min( begin, end )
  !end function

  function list_index_node( self, at, stride ) result(res)
    use adt_list__, only: List_t, ListNode_t, ListIndex_t
    implicit none
    type(List_t), target, intent(in) :: self
    type(ListNode_t),   target, optional :: at
    integer(kind=4),            optional :: stride
    type(ListIndex_t)                    :: res
    logical                              :: ok
    res%host => self
    if (present(at))   then; res%node => at
                       else; res%node => self%node%next
    end if
    if (present(stride))     res%stride = stride
  end function


!_PROC_EXPORT(listindex_index_idx)
  function listindex_index_idx( self, stride ) result(res)
    use adt_list__, only: ListIndex_t, list_index_node
    implicit none
    type(ListIndex_t), intent(in) :: self
    integer(kind=4),     optional :: stride
    type(ListIndex_t)             :: res
    res = list_index_node( self%host, self%node, stride )
  end function


!_PROC_EXPORT(list_index_int)
  function list_index_int( self, at, stride ) result(res)
    use adt_list__, only: List_t, ListIndex_t, listindex_advance_foot
    implicit none
    type(List_t),      target, intent(in) :: self
    integer(kind=4), optional, intent(in) :: at
    integer(kind=4), optional, intent(in) :: stride
    type(ListIndex_t)                     :: res
    logical                               :: ok
    res%host => self
    res%node => self%node
    if (present(at)) then; ok = listindex_advance_foot( res, at )
                     else; res%node => res%node%next
    end if
    if (present(stride))   res%stride = stride
  end function


!_PROC_EXPORT(listindex_set_prev)
  function listindex_set_prev( self ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(inout) :: self
    logical                          :: res
    res = listindex_advance_head( self, -self%stride )
  end function


!_PROC_EXPORT(listindex_prev)
  subroutine listindex_prev( self )
    use adt_list__; implicit none
    type(ListIndex_t), intent(inout) :: self
    logical                          :: ok
    ok = listindex_advance_head( self, -self%stride )
  end subroutine


!_PROC_EXPORT(listindex_get_prev)
  function listindex_get_prev( self ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(ListIndex_t)             :: res
    logical                       :: ok
    res = self
    ok = listindex_advance_head( res, -res%stride )
  end function


!_PROC_EXPORT(listindex_set_next)
  function listindex_set_next( self ) result(res)
    use adt_list__, only: ListIndex_t, listindex_advance_head
    implicit none
    type(ListIndex_t), intent(inout) :: self
    logical                          :: res
    res = listindex_advance_head( self, self%stride )
  end function


!_PROC_EXPORT(listindex_next)
  subroutine listindex_next( self )
    use adt_list__, only: ListIndex_t, listindex_advance_head
    implicit none
    type(ListIndex_t), intent(inout) :: self
    logical                          :: ok
    ok = listindex_advance_head( self, self%stride )
  end subroutine


!_PROC_EXPORT(listindex_get_next)
  function listindex_get_next( self ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(ListIndex_t)             :: res
    logical                       :: ok
    res = self
    ok = listindex_advance_head( res, res%stride )
  end function


  logical &
  function listindex_advance_foot( self, steps ) result(res)
    use adt_list__, only: ListIndex_t
    implicit none
    type(ListIndex_t), target, intent(inout) :: self
    integer(kind=4)                          :: steps, i

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
  function listindex_advance_head( self, steps ) result(res)
    use adt_list__, only: ListIndex_t
    implicit none
    type(ListIndex_t), target, intent(inout) :: self
    integer(kind=4)                          :: steps, i

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
  

!_PROC_EXPORT(listindex_is_valid)
  pure logical &
  function listindex_is_valid( self ) result(res)
    use adt_list__, only: ListIndex_t
    implicit none
    type(ListIndex_t), target, intent(in) :: self
    res = associated(self%host) .and. associated(self%node)
    if (res) &
      res = .not. associated(self%node, self%host%node)
  end function

  
!_PROC_EXPORT(listindex_eq_listindex)
  pure logical &
  function listindex_eq_listindex( self, other ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self, other
    res = associated( self%node, other%node )
  end function

  
!_PROC_EXPORT(listindex_eq_node)
  pure logical &
  function listindex_eq_node( self, node ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t),    intent(in) :: self
    type(ListNode_t), target, intent(in) :: node
    res = associated( self%node, node )
  end function

  
!_PROC_EXPORT(listindex_ne_listindex)
  pure logical &
  function listindex_ne_listindex( self, other ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self, other
    res = .not. associated( self%node, other%node )
  end function

  
!_PROC_EXPORT(listindex_ne_node)
  pure logical &
  function listindex_ne_node( self, node ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t),    intent(in) :: self
    type(ListNode_t), target, intent(in) :: node
    res = .not. associated( self%node, node )
  end function

  
!_PROC_EXPORT(listindex_insert_list)
  subroutine listindex_insert_list( self, lst )
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(List_t),          target :: lst
    call list_insert_nodes( self%node, lst%node, lst%node )
    self%host%length = self%host%length + lst%length
    lst%length       = 0
  end subroutine


!_PROC_EXPORT(listindex_insert_node)
  subroutine listindex_insert_node( self, node )
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(ListNode_t),          target :: node
    call list_link_node( node, self%node%prev, self%node )
    self%host%length = self%host%length + 1
  end subroutine


!_PROC_EXPORT(listindex_insert_idx)
  subroutine listindex_insert_idx( self, idx )
    use adt_list__, only: ListIndex_t, listindex_insert_idx_cnt
    implicit none
    type(ListIndex_t) :: self, idx
    integer(kind=4)   :: cnt
    call listindex_insert_idx_cnt( self, idx, cnt )
  end subroutine


  subroutine listindex_insert_idx_cnt( self, idx, cnt )
    use adt_list__, only: ListIndex_t, ListNode_t, listindex_is_valid, listindex_set_next, listindex_next, list_insert_nodes
    implicit none
    type(ListIndex_t)            :: self, idx
    integer(kind=4), intent(out) :: cnt
    type(ListNode_t),    pointer :: node
    logical                      :: done

    cnt = 0
    do while (listindex_is_valid( idx ))
      node => idx%node
      done = .not. listindex_set_next(idx)
      cnt  = cnt + 1
      call list_insert_nodes( self%node, node%prev, node%next )
      self%host%length = self%host%length + 1
      idx%host%length  = idx%host%length - 1

      if (done) then; exit
                else; call listindex_next(self)
      end if
    end do
  end subroutine


!_PROC_EXPORT(listindex_insert_range)
  subroutine listindex_insert_range( self, beg, end )
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self, beg, end
    call list_insert_nodes( self%node, beg%node%prev, end%node%next )
    self%host%length = -1
    beg%host%length  = -1
  end subroutine


!_PROC_EXPORT(listindex_remove_idx)
  subroutine listindex_remove_idx( self )
    use adt_list__; implicit none
    type(ListIndex_t) :: self
    call listindex_insert_idx( list_index_int(list_stale_list, tail, 0), self )
  end subroutine

  
!_PROC_EXPORT(list_pop_int)
  function list_pop_int( self, at ) result(res)
    use adt_list__; implicit none
    type(List_t), target :: self
    integer(kind=4)      :: at
    type(ListIndex_t)    :: res
    res = listindex_index_idx( listindex_pop_idx( list_index_int( self, at, 0 ) ), 0 )
  end function


!_PROC_EXPORT(listindex_pop_idx)
  function listindex_pop_idx( self ) result(res)
    use adt_list__, only: ListIndex_t, listindex_insert_idx_cnt, list_index_int, list_stale_list, tail
    implicit none
    type(ListIndex_t) :: self, res
    integer(kind=4)   :: cnt
    call listindex_insert_idx_cnt( list_index_int(list_stale_list, tail, 0), self, cnt )
    res = list_index_int( list_stale_list, -cnt )
  end function


!_PROC_EXPORT(list_assign_list)
  subroutine list_assign_list( lhs, rhs )
    use adt_list__, only: List_t, ListNode_t, NodeCloner, list_clear, list_append_node
    implicit none
    type(List_t), target, intent(inout) :: lhs
    type(List_t), target,    intent(in) :: rhs
    type(ListNode_t),               pointer :: copy, ptr, base
    procedure(NodeCloner),      pointer :: cloneNode

    base => rhs%node%next%prev !< sorry, but rhs is a shallow copy!
    if (.not. associated( base, lhs%node )) then
      call list_clear( lhs )
      ptr => base%next
      do while (.not. associated( ptr, base ))
        cloneNode => ptr%typeInfo%cloneObjProc
        call cloneNode( copy, ptr )
        call list_append_node( lhs, copy )
        ptr => ptr%next
      end do
    end if
  end subroutine


!_PROC_EXPORT(list_assign_idx)
  subroutine list_assign_idx( lhs, rhs )
    use adt_list__; implicit none
    type(List_t), target, intent(inout) :: lhs
    type(ListIndex_t),       intent(in) :: rhs
    type(ListIndex_t)                   :: idx
    type(ListNode_t),               pointer :: copy
    procedure(NodeCloner),      pointer :: cloneNode
    type(List_t)                        :: tmp

    idx = rhs
    if (associated( idx%host, lhs )) then
      call list_init( tmp )
      call listindex_insert_idx( list_index_int(tmp, tail, 0), idx )
      call list_clear( lhs )
      call list_append_list( lhs, tmp )
    else
      call list_clear( lhs )
      do while (listindex_is_valid(idx))
        cloneNode => idx%node%typeInfo%cloneObjProc
        call cloneNode( copy, idx%node )
        call list_append_node( lhs, copy )
        call listindex_next(idx)
      end do
    end if
  end subroutine

