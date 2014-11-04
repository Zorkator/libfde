
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

    pure logical function listidx_is_valid( self )
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

    subroutine listidx_insert_idx( self, idx )
      import ListIndex_t
      type(ListIndex_t) :: self, idx
    end subroutine

    subroutine listidx_insert_idx_cnt( self, idx, cnt )
      import ListIndex_t
      type(ListIndex_t)            :: self, idx
      integer(kind=4), intent(out) :: cnt
    end subroutine

    logical &
    function listidx_advance_head( self, steps )
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
      integer(kind=4)                  :: steps
    end function

    logical &
    function listidx_advance_foot( self, steps )
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

    function listidx_pop_idx( self ) result(res)
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

    function listidx_set_next( self ) result(res)
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
      logical                          :: res
    end function

    function listidx_index_idx( self, stride ) result(res)
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

    subroutine listidx_next( self )
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
    call listidx_insert_idx( list_index_int(self, tail, 0), idx )
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


!_PROC_EXPORT(listidx_dynamic_type)
  function listidx_dynamic_type( self ) result(res)
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


!_PROC_EXPORT(listidx_index_idx)
  function listidx_index_idx( self, stride ) result(res)
    use adt_list__, only: ListIndex_t, list_index_node
    implicit none
    type(ListIndex_t), intent(in) :: self
    integer(kind=4),     optional :: stride
    type(ListIndex_t)             :: res
    res = list_index_node( self%host, self%node, stride )
  end function


!_PROC_EXPORT(list_index_int)
  function list_index_int( self, at, stride ) result(res)
    use adt_list__, only: List_t, ListIndex_t, listidx_advance_foot
    implicit none
    type(List_t),      target, intent(in) :: self
    integer(kind=4), optional, intent(in) :: at
    integer(kind=4), optional, intent(in) :: stride
    type(ListIndex_t)                     :: res
    logical                               :: ok
    res%host => self
    res%node => self%node
    if (present(at)) then; ok = listidx_advance_foot( res, at )
                     else; res%node => res%node%next
    end if
    if (present(stride))   res%stride = stride
  end function


!_PROC_EXPORT(listidx_set_prev)
  function listidx_set_prev( self ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(inout) :: self
    logical                          :: res
    res = listidx_advance_head( self, -self%stride )
  end function


!_PROC_EXPORT(listidx_prev)
  subroutine listidx_prev( self )
    use adt_list__; implicit none
    type(ListIndex_t), intent(inout) :: self
    logical                          :: ok
    ok = listidx_advance_head( self, -self%stride )
  end subroutine


!_PROC_EXPORT(listidx_get_prev)
  function listidx_get_prev( self ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(ListIndex_t)             :: res
    logical                       :: ok
    res = self
    ok = listidx_advance_head( res, -res%stride )
  end function


!_PROC_EXPORT(listidx_set_next)
  function listidx_set_next( self ) result(res)
    use adt_list__, only: ListIndex_t, listidx_advance_head
    implicit none
    type(ListIndex_t), intent(inout) :: self
    logical                          :: res
    res = listidx_advance_head( self, self%stride )
  end function


!_PROC_EXPORT(listidx_next)
  subroutine listidx_next( self )
    use adt_list__, only: ListIndex_t, listidx_advance_head
    implicit none
    type(ListIndex_t), intent(inout) :: self
    logical                          :: ok
    ok = listidx_advance_head( self, self%stride )
  end subroutine


!_PROC_EXPORT(listidx_get_next)
  function listidx_get_next( self ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(ListIndex_t)             :: res
    logical                       :: ok
    res = self
    ok = listidx_advance_head( res, res%stride )
  end function


  logical &
  function listidx_advance_foot( self, steps ) result(res)
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
  function listidx_advance_head( self, steps ) result(res)
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
  

!_PROC_EXPORT(listidx_is_valid)
  pure logical &
  function listidx_is_valid( self ) result(res)
    use adt_list__, only: ListIndex_t
    implicit none
    type(ListIndex_t), target, intent(in) :: self
    res = associated(self%host) .and. associated(self%node)
    if (res) &
      res = .not. associated(self%node, self%host%node)
  end function

  
!_PROC_EXPORT(listidx_eq_listidx)
  pure logical &
  function listidx_eq_listidx( self, other ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self, other
    res = associated( self%node, other%node )
  end function

  
!_PROC_EXPORT(listidx_eq_node)
  pure logical &
  function listidx_eq_node( self, node ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t),    intent(in) :: self
    type(ListNode_t), target, intent(in) :: node
    res = associated( self%node, node )
  end function

  
!_PROC_EXPORT(listidx_ne_listidx)
  pure logical &
  function listidx_ne_listidx( self, other ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self, other
    res = .not. associated( self%node, other%node )
  end function

  
!_PROC_EXPORT(listidx_ne_node)
  pure logical &
  function listidx_ne_node( self, node ) result(res)
    use adt_list__; implicit none
    type(ListIndex_t),    intent(in) :: self
    type(ListNode_t), target, intent(in) :: node
    res = .not. associated( self%node, node )
  end function

  
!_PROC_EXPORT(listidx_insert_list)
  subroutine listidx_insert_list( self, lst )
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(List_t),          target :: lst
    call list_insert_nodes( self%node, lst%node, lst%node )
    self%host%length = self%host%length + lst%length
    lst%length       = 0
  end subroutine


!_PROC_EXPORT(listidx_insert_node)
  subroutine listidx_insert_node( self, node )
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self
    type(ListNode_t),          target :: node
    call list_link_node( node, self%node%prev, self%node )
    self%host%length = self%host%length + 1
  end subroutine


!_PROC_EXPORT(listidx_insert_idx)
  subroutine listidx_insert_idx( self, idx )
    use adt_list__, only: ListIndex_t, listidx_insert_idx_cnt
    implicit none
    type(ListIndex_t) :: self, idx
    integer(kind=4)   :: cnt
    call listidx_insert_idx_cnt( self, idx, cnt )
  end subroutine


  subroutine listidx_insert_idx_cnt( self, idx, cnt )
    use adt_list__, only: ListIndex_t, ListNode_t, listidx_is_valid, listidx_set_next, listidx_next, list_insert_nodes
    implicit none
    type(ListIndex_t)            :: self, idx
    integer(kind=4), intent(out) :: cnt
    type(ListNode_t),    pointer :: node
    logical                      :: done

    cnt = 0
    do while (listidx_is_valid( idx ))
      node => idx%node
      done = .not. listidx_set_next(idx)
      cnt  = cnt + 1
      call list_insert_nodes( self%node, node%prev, node%next )
      self%host%length = self%host%length + 1
      idx%host%length  = idx%host%length - 1

      if (done) then; exit
                else; call listidx_next(self)
      end if
    end do
  end subroutine


!_PROC_EXPORT(listidx_insert_range)
  subroutine listidx_insert_range( self, beg, end )
    use adt_list__; implicit none
    type(ListIndex_t), intent(in) :: self, beg, end
    call list_insert_nodes( self%node, beg%node%prev, end%node%next )
    self%host%length = -1
    beg%host%length  = -1
  end subroutine


!_PROC_EXPORT(listidx_remove_idx)
  subroutine listidx_remove_idx( self )
    use adt_list__; implicit none
    type(ListIndex_t) :: self
    call listidx_insert_idx( list_index_int(list_stale_list, tail, 0), self )
  end subroutine

  
!_PROC_EXPORT(list_pop_int)
  function list_pop_int( self, at ) result(res)
    use adt_list__; implicit none
    type(List_t), target :: self
    integer(kind=4)      :: at
    type(ListIndex_t)    :: res
    res = listidx_index_idx( listidx_pop_idx( list_index_int( self, at, 0 ) ), 0 )
  end function


!_PROC_EXPORT(listidx_pop_idx)
  function listidx_pop_idx( self ) result(res)
    use adt_list__, only: ListIndex_t, listidx_insert_idx_cnt, list_index_int, list_stale_list, tail
    implicit none
    type(ListIndex_t) :: self, res
    integer(kind=4)   :: cnt
    call listidx_insert_idx_cnt( list_index_int(list_stale_list, tail, 0), self, cnt )
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
      call listidx_insert_idx( list_index_int(tmp, tail, 0), idx )
      call list_clear( lhs )
      call list_append_list( lhs, tmp )
    else
      call list_clear( lhs )
      do while (listidx_is_valid(idx))
        cloneNode => idx%node%typeInfo%cloneObjProc
        call cloneNode( copy, idx%node )
        call list_append_node( lhs, copy )
        call listidx_next(idx)
      end do
    end if
  end subroutine

