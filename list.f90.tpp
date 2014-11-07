
#include "adt/itfUtil.fpp"

module adt_list
  use adt_ref
  implicit none
  private

  type, public :: ListNode_t
    private
    type(TypeInfo_t), public, pointer :: typeInfo => null()
    type(ListNode_t),         pointer :: prev => null(), next => null()
    type(void_t)                      :: padding
  end type


  type, public :: List_t
    private
    type(ListNode_t) :: node
    integer(kind=4)  :: length   =  0
  end type


  type, public :: ListIndex_t
    private
    type (ListNode_t), public, pointer :: node   => null()
    type (List_t),             pointer :: host   => null()
    integer(kind=4)                    :: stride = 1
  end type


  !_TypeGen_declare_RefType( public, List, type(List_t), scalar, \
  !     initProc   = list_init_by_proto_c, \
  !     deleteProc = list_delete_c,        \
  !     assignProc = list_assign_list_c,   \
  !     cloneMode  = _type )

  !_TypeGen_declare_ListNode( public, List, type(List_t), scalar )


  integer(kind=4), parameter, public :: first = 1, last = -1, tail = 0

  public :: initialize
  public :: len
  public :: is_valid
  public :: is_empty   
  public :: append
  public :: delete
  public :: clear
  public :: dynamic_type
  public :: index
  public :: prev, set_prev, get_prev
  public :: next, set_next, get_next
  public :: insert
  public :: remove
  public :: pop
  public :: operator(==), operator(/=)
  public :: assign, assignment(=)

  
  interface initialize
    subroutine list_init_by_proto_c( self, has_proto, proto )
      import List_t
      type(List_t)    :: self, proto
      integer(kind=4) :: has_proto
    end subroutine

    subroutine list_init_c( self )
      import List_t
      type(List_t) :: self
    end subroutine
  end interface

  interface len
    function list_length_c( self ) result(res)
      import List_t
      type(List_t)    :: self
      integer(kind=4) :: res
    end function
  end interface

  interface is_valid
    pure logical function listindex_is_valid_c( self )
      import ListIndex_t
      type(ListIndex_t), intent(in) :: self
    end function

    pure logical function list_is_valid_c( self )
      import List_t
      type(List_t), intent(in) :: self
    end function
  end interface

  interface is_empty
    pure logical function list_is_empty_c( self )
      import List_t
      type (List_t), intent(in) :: self
    end function
  end interface

  interface append
    subroutine list_append_list_c( self, other )
      import List_t
      type (List_t), target :: self, other
    end subroutine
    
    subroutine list_append_node_c( self, node )
      import List_t, ListNode_t
      type (List_t), target :: self
      type (ListNode_t), target :: node
    end subroutine
    
    subroutine list_append_idx_c( self, idx )
      import List_t, ListIndex_t
      type (List_t), target :: self
      type (ListIndex_t)    :: idx
    end subroutine
  end interface

  interface delete
    subroutine list_delete_c( self )
      import List_t
      type (List_t), intent(inout) :: self
    end subroutine
  end interface
  
  interface clear
    recursive &
    subroutine list_clear_c( self )
      import List_t
      type (List_t), intent(inout) :: self
    end subroutine
  end interface
  
  interface dynamic_type
    function listindex_dynamic_type( self ) result(res)
      import ListIndex_t, TypeInfo_t
      type(ListIndex_t), intent(in) :: self
      type(TypeInfo_t),     pointer :: res
    end function
  end interface

  interface index       
    function listindex_index_idx( self, stride ) result(res)
      import ListIndex_t
      type(ListIndex_t), intent(in) :: self
      integer(kind=4),     optional :: stride
      type(ListIndex_t)             :: res
    end function
    
    function list_index_int( self, at, stride ) result(res)
      import List_t, ListIndex_t
      type(List_t),      target, intent(in) :: self
      integer(kind=4), optional, intent(in) :: at
      integer(kind=4), optional, intent(in) :: stride
      type(ListIndex_t)                     :: res
    end function
  end interface

  interface prev        
    subroutine listindex_prev_c( self )
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
    end subroutine
  end interface

  interface next        
    subroutine listindex_next_c( self )
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
    end subroutine
  end interface

  interface set_prev    
    function listindex_set_prev_c( self ) result(res)
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
      logical                          :: res
    end function
  end interface

  interface set_next    
    function listindex_set_next_c( self ) result(res)
      import ListIndex_t
      type(ListIndex_t), intent(inout) :: self
      logical                          :: res
    end function
  end interface

  interface get_prev    
    function listindex_get_prev( self ) result(res)
      import ListIndex_t
      type(ListIndex_t), intent(in) :: self
      type(ListIndex_t)             :: res
    end function
  end interface

  interface get_next    
    function listindex_get_next( self ) result(res)
      import ListIndex_t
      type(ListIndex_t), intent(in) :: self
      type(ListIndex_t)             :: res
    end function
  end interface

  interface insert      
    subroutine listindex_insert_list_c( self, list )
      import List_t, ListIndex_t
      type(ListIndex_t), intent(in) :: self
      type(List_t),          target :: list
    end subroutine
    
    subroutine listindex_insert_node_c( self, node )
      import ListIndex_t, ListNode_t
      type(ListIndex_t), intent(in) :: self
      type(ListNode_t),      target :: node
    end subroutine
    
    subroutine listindex_insert_idx_c( self, idx )
      import ListIndex_t
      type(ListIndex_t) :: self, idx
    end subroutine

    subroutine listindex_insert_range_c( self, beg, end )
      import ListIndex_t
      type(ListIndex_t), intent(in) :: self, beg, end
    end subroutine
  end interface

  interface remove      
    subroutine listindex_remove_idx_c( self )
      import ListIndex_t
      type(ListIndex_t) :: self
    end subroutine
  end interface

  interface pop         
    function list_pop_int( self, at ) result(res)
      import List_t, ListIndex_t
      type(List_t), target :: self
      integer(kind=4)      :: at
      type(ListIndex_t)    :: res
    end function
    
    function listindex_pop_idx( self ) result(res)
      import ListIndex_t
      type(ListIndex_t) :: self, res
    end function
  end interface

  interface assign        
    subroutine list_assign_list_c( lhs, rhs )
      import List_t
      type(List_t), intent(inout) :: lhs
      type(List_t),    intent(in) :: rhs
    end subroutine
    
    subroutine list_assign_idx_c( lhs, rhs )
      import List_t, ListIndex_t
      type(List_t),   intent(inout) :: lhs
      type(ListIndex_t), intent(in) :: rhs
    end subroutine
  end interface

  interface assignment(=) 
    module procedure list_assign_list_private, list_assign_idx_private                   
  end interface

  interface operator(==)  
    pure logical function listindex_eq_listindex_c( self, other )
      import ListIndex_t
      type(ListIndex_t), intent(in) :: self, other
    end function
    
    pure logical function listindex_eq_node_c( self, node )
      import ListIndex_t, ListNode_t
      type(ListIndex_t), intent(in) :: self
      type(ListNode_t),      intent(in) :: node
    end function
  end interface

  interface operator(/=)  
    pure logical function listindex_ne_listindex_c( self, other )
      import ListIndex_t
      type(ListIndex_t), intent(in) :: self, other
    end function
    
    pure logical function listindex_ne_node_c( self, node )
      import ListIndex_t, ListNode_t
      type(ListIndex_t), intent(in) :: self
      type(ListNode_t),      intent(in) :: node
    end function
  end interface

!-----------------
  contains
!-----------------

  !_TypeGen_implementAll()

!_PROC_EXPORT(list_assign_list_private)
  subroutine list_assign_list_private( lhs, rhs )
    type(List_t), intent(inout) :: lhs
    type(List_t),    intent(in) :: rhs
    call assign( lhs, rhs )
  end subroutine
  
!_PROC_EXPORT(list_assign_idx_private)
  subroutine list_assign_idx_private( lhs, rhs )
    type(List_t),   intent(inout) :: lhs
    type(ListIndex_t), intent(in) :: rhs
    call assign( lhs, rhs )
  end subroutine
end module

