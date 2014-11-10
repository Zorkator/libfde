
#include "adt/itfUtil.fpp"

module adt_hashmap
  use adt_string
  use adt_list
  use adt_item
  use adt_ref
  use iso_c_binding
  implicit none
  private

  integer, parameter :: default_indexLimits(2) = (/ 10, 100000 /)

  type, public :: HashMap_t
    private
    type(List_t), dimension(:), pointer :: indexVector    => null()
    integer                             :: items          =  0, resize_cnt = 0
    integer                             :: indexLimits(2) =  default_indexLimits
  end type

  !_TypeGen_declare_RefType( public, HashMap, type(HashMap_t), scalar, \
  !     initProc   = hashmap_init_by_proto_c,  \
  !     assignProc = hashmap_assign_hashmap_c, \
  !     deleteProc = hashmap_delete_c,         \
  !     cloneMode  = _type )

  !_TypeGen_declare_ListNode( public, HashMap, type(HashMap_t), scalar )

  public :: initialize
  public :: len
  public :: clear
  public :: delete
  public :: set, get, remove, unset, pop
  public :: setDefault, hasKey
  public :: hashmap_clear_cache, hashmap_get_stats
  public :: assign, assignment(=)
  public :: default_indexLimits

  interface initialize
    subroutine hashmap_init_by_proto_c( self, has_proto, proto )
      import HashMap_t
      type(HashMap_t) :: self, proto
      integer         :: has_proto
    end subroutine

    subroutine hashmap_init_c( self )
      import HashMap_t
      type(HashMap_t), intent(inout) :: self
    end subroutine

    subroutine hashmap_init_sized_c( self, index_min, index_max )
      import HashMap_t
      type(HashMap_t), intent(inout) :: self
      integer                        :: index_min, index_max
    end subroutine
  end interface

  interface len
    pure &
    function hashmap_len_c( self ) result(res)
      import HashMap_t
      type(HashMap_t), intent(in) :: self
      integer(kind=4)             :: res
    end function
  end interface

  interface clear
    subroutine hashmap_clear_c( self )
      import HashMap_t
      type(HashMap_t), intent(inout) :: self
    end subroutine
  end interface

  interface delete
    subroutine hashmap_delete_c( self )
      import HashMap_t
      type(HashMap_t), intent(inout) :: self
    end subroutine
  end interface

  interface assign
    subroutine hashmap_assign_hashmap_c( lhs, rhs )
      import HashMap_t
      type(HashMap_t), intent(inout) :: lhs
      type(HashMap_t),    intent(in) :: rhs
    end subroutine  
  end interface

  interface assignment(=)
    module procedure hashmap_assign_hashmap_private
  end interface

  interface set
    subroutine hashmap_set_c( self, key, val )
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

  interface getPtr
    function hashmap_get_ptr( self, key ) result(res)
      import HashMap_t, Item_t
      type(HashMap_t)              :: self
      character(len=*), intent(in) :: key
      type(Item_t),        pointer :: res
    end function
  end interface

  interface remove
    subroutine hashmap_remove_key_c( self, key )
      import HashMap_t
      type(HashMap_t), intent(inout) :: self
      character(len=*),   intent(in) :: key
    end subroutine
  end interface

  interface unset
    logical &
    function hashmap_unset_key_c( self, key ) result(res)
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
      type(Item_t)                       :: defaultVal
      type(Item_t),              pointer :: res
    end function
  end interface
  
  interface hasKey
    logical &
    function hashmap_has_key_c( self, key )
      import HashMap_t
      type(HashMap_t)              :: self
      character(len=*), intent(in) :: key
    end function
  end interface

  interface hashmap_clear_cache
    subroutine hashmap_clear_cache_c()
    end subroutine
  end interface

  interface hashmap_get_stats
    subroutine hashmap_get_stats_c( self, stats )
      import HashMap_t
      type(HashMap_t), intent(in) :: self
      integer                     :: stats(6)
    end subroutine
  end interface

contains

  !_TypeGen_implementAll()

!_PROC_EXPORT(hashmap_assign_hashmap_private)
  subroutine hashmap_assign_hashmap_private( lhs, rhs )
    type(HashMap_t), intent(inout) :: lhs
    type(HashMap_t),    intent(in) :: rhs
    call assign( lhs, rhs )
  end subroutine  

end module

