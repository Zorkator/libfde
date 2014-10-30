
#include "adt/itfUtil.fpp"

module adt_list
  use adt_ref
  implicit none
  private

  type, public :: Node_t
    type(Node_t),     pointer :: prev => null(), next => null()
    type(TypeInfo_t), pointer :: typeInfo => null()
		type(void_t)              :: padding
  end type

  type, private :: ValueNode_t
    private
    type(Node_t) :: super
    integer      :: pseudoValue
  end type

  type, public :: List_t
    private
    type(Node_t) :: node
    integer*4    :: length   =  0
  end type

  type, public :: ListIndex_t
    private
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
  end interface


  interface initialize  ; module procedure al_initialize_list                              ; end interface
  interface len         ; module procedure al_length                                       ; end interface
  interface is_valid    ; module procedure al_is_valid, ali_is_valid                       ; end interface
  interface is_empty    ; module procedure al_is_empty                                     ; end interface
  interface append      ; module procedure al_append_list, al_append_node, al_append_idx   ; end interface
  interface clear       ; module procedure al_delete                                       ; end interface
  interface delete      ; module procedure al_delete                                       ; end interface
  interface dynamic_type; module procedure ali_dynamic_type                                ; end interface

  interface index       ; module procedure ali_index_idx, al_index_int;                    ; end interface
  interface prev        ; module procedure ali_prev                                        ; end interface
  interface next        ; module procedure ali_next                                        ; end interface
  interface set_prev    ; module procedure ali_set_prev                                    ; end interface
  interface set_next    ; module procedure ali_set_next                                    ; end interface
  interface get_prev    ; module procedure ali_get_prev                                    ; end interface
  interface get_next    ; module procedure ali_get_next                                    ; end interface
  interface insert      ; module procedure ali_insert_list, ali_insert_node, ali_insert_idx; end interface
  interface insert      ; module procedure ali_insert_range                                ; end interface
  interface remove      ; module procedure ali_remove_idx                                  ; end interface
  interface pop         ; module procedure ali_pop_idx, al_pop_int                     ; end interface

  interface assign        ; module procedure al_assign_al, al_assign_idx                   ; end interface
  interface assignment(=) ; module procedure al_assign_al, al_assign_idx                   ; end interface
  interface operator(==)  ; module procedure ali_eq_ali, ali_eq_node                       ; end interface
  interface operator(/=)  ; module procedure ali_ne_ali, ali_ne_node                       ; end interface

  integer*4, parameter :: first = 1, last = -1, tail = 0

  public :: initialize
  public :: len
  public :: is_valid
  public :: is_empty   
  public :: append
  public :: delete
  public :: clear
  public :: dynamic_type

  public :: index
  public :: first, last, tail
  public :: prev, set_prev, get_prev
  public :: next, set_next, get_next
  public :: insert
  public :: remove
  public :: pop

  public :: operator(==), operator(/=)
  public :: assign, assignment(=)

  !public :: al_index

  !_TypeGen_declare_RefType( public, List, type(List_t), scalar, \
  !     initProc   = al_initialize, \
  !     deleteProc = al_delete,     \
  !     assignProc = al_assign_al,  \
  !     cloneMode  = _type )

  !_TypeGen_declare_ListNode( public, List, type(List_t), scalar )


!-----------------
  contains
!-----------------

  !_TypeGen_implementAll()

!_PROC_EXPORT(al_initialize_node)
  subroutine al_initialize_node( self, nodeType )
    type (Node_t),              target :: self
    type(TypeInfo_t), optional, target :: nodeType
    self%prev     => self
    self%next     => self
    self%typeInfo => nodeType
  end subroutine

  subroutine al_link_node( node, prev, next )
    type (Node_t), target :: node, prev, next
    next%prev => node
    node%next => next
    node%prev => prev
    prev%next => node
  end subroutine

  subroutine al_unlink_node( prev, next )
    type (Node_t), target :: prev, next
    prev%next => next
    next%prev => prev
  end subroutine

  subroutine al_remove_node( node )
    type (Node_t), target :: node
    call al_unlink_node( node%prev, node%next )
    node%prev => null()
    node%next => null()
  end subroutine

  subroutine al_replace_node( old, new )
    type (Node_t), target :: old, new
    call al_unlink_node( old%prev, old%next )
    call al_link_node( new, old%prev, old%next )
    old%prev => null()
    old%next => null()
  end subroutine

  subroutine al_insert_nodes( pos_hook, beg_prev, end_next )
    type(Node_t), target :: pos_hook, beg_prev, end_next
    pos_hook % prev%next => beg_prev % next
    beg_prev % next%prev => pos_hook % prev
    pos_hook % prev      => end_next % prev
    end_next % prev%next => pos_hook
    call al_unlink_node( beg_prev, end_next )
  end subroutine


  subroutine al_initialize( self, has_proto, proto )
    type(List_t), target :: self
    integer              :: has_proto
    type(List_t)         :: proto
    call al_initialize_list( self )
  end subroutine


  subroutine al_initialize_list( self )
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
    type(List_t), target, intent(in) :: self
    res = associated(self%node%prev) .and. associated(self%node%next)
  end function


  pure logical function al_is_empty( self ) result(res)
    type (List_t), target, intent(in) :: self
    res = associated(self%node%prev, self%node) .and. associated(self%node%next, self%node)
  end function


  subroutine al_append_list( self, other )
    type (List_t), target :: self, other
    call al_insert_nodes( self%node, other%node, other%node )
    self%length  = self%length + other%length
    other%length = 0
  end subroutine


  subroutine al_append_node( self, node )
    type (List_t), target :: self
    type (Node_t), target :: node
    call al_link_node( node, self%node%prev, self%node )
    self%length = self%length + 1
  end subroutine


  subroutine al_append_idx( self, idx )
    type (List_t), target :: self
    type (ListIndex_t)    :: idx
    call ali_insert_idx( index(self, tail, 0), idx )
  end subroutine


  subroutine al_delete( self )
    type (List_t), target, intent(inout) :: self
    call al_delete_list( self )
    call al_delete_list( al_stale_list )
  end subroutine

  
!_PROC_EXPORT(al_delete_list)
  recursive &
  subroutine al_delete_list( self )
    use iso_c_binding
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
    type(ListIndex_t), intent(in) :: self
    integer*4,           optional :: stride
    type(ListIndex_t)             :: res
    res = al_index_node( self%host, self%node, stride )
  end function


  function al_index_int( self, at, stride ) result(res)
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
    type(ListIndex_t), intent(inout) :: self
    logical                          :: res
    res = ali_advance_head( self, -self%stride )
  end function


  subroutine ali_prev( self )
    type(ListIndex_t), intent(inout) :: self
    logical                          :: ok
    ok = ali_advance_head( self, -self%stride )
  end subroutine


  function ali_get_prev( self ) result(res)
    type(ListIndex_t), intent(in) :: self
    type(ListIndex_t)             :: res
    logical                       :: ok
    res = self
    ok = ali_advance_head( res, -res%stride )
  end function


  function ali_set_next( self ) result(res)
    type(ListIndex_t), intent(inout) :: self
    logical                          :: res
    res = ali_advance_head( self, self%stride )
  end function


  subroutine ali_next( self )
    type(ListIndex_t), intent(inout) :: self
    logical                          :: ok
    ok = ali_advance_head( self, self%stride )
  end subroutine


  function ali_get_next( self ) result(res)
    type(ListIndex_t), intent(in) :: self
    type(ListIndex_t)             :: res
    logical                       :: ok
    res = self
    ok = ali_advance_head( res, res%stride )
  end function


  logical &
  function ali_advance_foot( self, steps ) result(res)
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
    type(ListIndex_t), target, intent(in) :: self
    res = associated(self%host) .and. associated(self%node)
    if (res) &
      res = .not. associated(self%node, self%host%node)
  end function

  
  pure logical &
  function ali_eq_ali( self, other ) result(res)
    type(ListIndex_t), intent(in) :: self, other
    res = associated( self%node, other%node )
  end function

  
  pure logical &
  function ali_eq_node( self, node ) result(res)
    type(ListIndex_t),    intent(in) :: self
    type(Node_t), target, intent(in) :: node
    res = associated( self%node, node )
  end function

  
  pure logical &
  function ali_ne_ali( self, other ) result(res)
    type(ListIndex_t), intent(in) :: self, other
    res = .not. associated( self%node, other%node )
  end function

  
  pure logical &
  function ali_ne_node( self, node ) result(res)
    type(ListIndex_t),    intent(in) :: self
    type(Node_t), target, intent(in) :: node
    res = .not. associated( self%node, node )
  end function

  
  subroutine ali_insert_list( self, list )
    type(ListIndex_t), intent(in) :: self
    type(List_t),          target :: list
    call al_insert_nodes( self%node, list%node, list%node )
    self%host%length = self%host%length + list%length
    list%length      = 0
  end subroutine


  subroutine ali_insert_node( self, node )
    type(ListIndex_t), intent(in) :: self
    type(Node_t),          target :: node
    call al_link_node( node, self%node%prev, self%node )
    self%host%length = self%host%length + 1
  end subroutine


  subroutine ali_insert_idx( self, idx )
    type(ListIndex_t) :: self, idx
    integer*4         :: cnt
    call ali_insert_idx_cnt( self, idx, cnt )
  end subroutine


  subroutine ali_insert_idx_cnt( self, idx, cnt )
    type(ListIndex_t)      :: self, idx
    integer*4, intent(out) :: cnt
    type(Node_t),  pointer :: node
    logical                :: done

    cnt = 0
    do while (is_valid( idx ))
      node => idx%node
      done = .not. set_next(idx)
      cnt  = cnt + 1
      call al_insert_nodes( self%node, node%prev, node%next )
      self%host%length = self%host%length + 1
      idx%host%length  = idx%host%length - 1

      if (done) then; exit
                else; call next(self)
      end if
    end do
  end subroutine


  subroutine ali_insert_range( self, beg, end )
    type(ListIndex_t), intent(in) :: self, beg, end
    call al_insert_nodes( self%node, beg%node%prev, end%node%next )
    self%host%length = -1
    beg%host%length  = -1
  end subroutine


  subroutine ali_remove_idx( self )
    type(ListIndex_t) :: self
    call ali_insert_idx( index(al_stale_list, tail, 0), self )
  end subroutine

  
!_PROC_EXPORT(al_pop_int)
  function al_pop_int( self, at ) result(res)
    type(List_t), target :: self
    integer*4            :: at
    type(ListIndex_t)    :: res
    res = index( ali_pop_idx( index( self, at, 0 ) ), 0 )
  end function


!_PROC_EXPORT(ali_pop_idx)
  function ali_pop_idx( self ) result(res)
    type(ListIndex_t) :: self, res
    integer*4         :: cnt
    call ali_insert_idx_cnt( index(al_stale_list, tail, 0), self, cnt )
    res = index( al_stale_list, -cnt )
  end function


!_PROC_EXPORT(al_assign_al)
  subroutine al_assign_al( lhs, rhs )
    type(List_t), target, intent(inout) :: lhs
    type(List_t), target,    intent(in) :: rhs
    type(Node_t),               pointer :: copy, ptr, base
    procedure(NodeCloner),      pointer :: cloneNode

    base => rhs%node%next%prev !< sorry, but rhs is a shallow copy!
    if (.not. associated( base, lhs%node )) then
      call clear( lhs )
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
    type(List_t), target, intent(inout) :: lhs
    type(ListIndex_t),       intent(in) :: rhs
    type(ListIndex_t)                   :: idx
    type(Node_t),               pointer :: copy
    procedure(NodeCloner),      pointer :: cloneNode
    type(List_t)                        :: tmp

    idx = rhs
    if (associated( idx%host, lhs )) then
      call initialize( tmp )
      call ali_insert_idx( index(tmp, tail, 0), idx )
      call clear( lhs )
      call al_append_list( lhs, tmp )
    else
      call clear( lhs )
      do while (is_valid(idx))
        cloneNode => idx%node%typeInfo%cloneObjProc
        call cloneNode( copy, idx%node )
        call al_append_node( lhs, copy )
        call next(idx)
      end do
    end if
  end subroutine

end module

