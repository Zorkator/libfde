
module adt_hashmap
  use adt_string
  use adt_list
  use adt_item
  use adt_ref
  implicit none
  private

  integer, parameter :: default_indexLimits(2) = (/ 10, 100000 /)

  type, public :: HashMap_t
    private
    type(List_t), dimension(:), pointer :: indexVector    => null()
    integer*4                           :: items          =  0
    integer*4                           :: indexLimits(2) = default_indexLimits
  end type

  !_TypeGen_declare_RefType( public, HashMap, type(HashMap_t), scalar, \
  !     initProc   = hashmap_init_by_proto,  \
  !     assignProc = hashmap_assign_hashmap, \
  !     deleteProc = hashmap_delete,         \
  !     cloneMode  = _type )

  !_TypeGen_declare_ListNode( public, HashMap, type(HashMap_t), scalar )

  public :: initialize
  public :: len
  public :: clear
  public :: delete
  public :: set, get, remove, unset, pop
  public :: setDefault, hasKey
  public :: hashmap_clear_cache
  public :: assign, assignment(=)
  public :: default_indexLimits

  interface initialize
    subroutine hashmap_init_by_proto( self, has_proto, proto )
      import HashMap_t
      type(HashMap_t) :: self, proto
      integer         :: has_proto
    end subroutine

    subroutine hashmap_init( self )
      import HashMap_t
      type(HashMap_t), intent(inout) :: self
    end subroutine

    subroutine hashmap_init_sized( self, index_min, index_max )
      import HashMap_t
      type(HashMap_t), intent(inout) :: self
      integer                        :: index_min, index_max
    end subroutine
  end interface

  interface len
    pure &
    function hashmap_len( self ) result(res)
      import HashMap_t
      type(HashMap_t), intent(in) :: self
      integer*4                   :: res
    end function
  end interface

  interface clear
    subroutine hashmap_clear( self, tgtList )
      import HashMap_t, List_t
      type(HashMap_t), intent(inout) :: self
      type(List_t),         optional :: tgtList
    end subroutine
  end interface

  interface delete
    subroutine hashmap_delete( self )
      import HashMap_t
      type(HashMap_t), intent(inout) :: self
    end subroutine
  end interface

  interface assign
    subroutine hashmap_assign_hashmap( lhs, rhs )
      import HashMap_t
      type(HashMap_t), intent(inout) :: lhs
      type(HashMap_t),    intent(in) :: rhs
    end subroutine  
  end interface

  interface assignment(=)
    module procedure hashmap_assign_hashmap_private
  end interface

  interface set
    subroutine hashmap_set( self, key, val )
      import HashMap_t, Item_t
      type(HashMap_t), intent(inout) :: self
      character(len=*),   intent(in) :: key
      type(Item_t),       intent(in) :: val
    end subroutine
  end interface

  interface get
    function hashmap_get( self, key ) result(res)
      import HashMap_t, Item_t
      type(HashMap_t)              :: self
      character(len=*), intent(in) :: key
      type(Item_t),        pointer :: res
    end function
  end interface

  interface remove
    subroutine hashmap_remove_key( self, key )
      import HashMap_t
      type(HashMap_t), intent(inout) :: self
      character(len=*),   intent(in) :: key
    end subroutine
  end interface

  interface unset
    logical &
    function hashmap_unset_key( self, key ) result(res)
      import HashMap_t
      type(HashMap_t), intent(inout) :: self
      character(len=*),   intent(in) :: key
    end function
  end interface

  interface pop
    function hashmap_pop_key( self, key ) result(res)
      import HashMap_t, Item_t
      type(HashMap_t), intent(inout) :: self
      character(len=*),   intent(in) :: key
      type(Item_t),          pointer :: res
    end function
  end interface

  interface setDefault
    function hashmap_set_default( self, key, defaultVal ) result(res)
      import HashMap_t, Item_t
      type(HashMap_t),     intent(inout) :: self
      character(len=*),       intent(in) :: key
      type(Item_t), optional, intent(in) :: defaultVal
      type(Item_t),              pointer :: res
    end function
  end interface
  
  interface hasKey
    logical &
    function hashmap_has_key( self, key )
      import HashMap_t
      type(HashMap_t)              :: self
      character(len=*), intent(in) :: key
    end function
  end interface

  interface
    subroutine hashmap_clear_cache()
    end subroutine

    function hashmap_get_value_ref( self, key, clearStale ) result(res)
      import HashMap_t, Item_t
      type(HashMap_t)              :: self
      character(len=*), intent(in) :: key
      logical                      :: clearStale
      type(Item_t),        pointer :: res
    end function
    
    function hashmap_get_value_ptr( self, key ) result(res)
      import HashMap_t, Item_t
      type(HashMap_t)              :: self
      character(len=*), intent(in) :: key
      type(Item_t),        pointer :: res
    end function
  end interface

contains

  !_TypeGen_implementAll()

  subroutine hashmap_assign_hashmap_private( lhs, rhs )
    type(HashMap_t), intent(inout) :: lhs
    type(HashMap_t),    intent(in) :: rhs
    call assign( lhs, rhs )
  end subroutine  

end module

