
#include "adt/itfUtil.fpp"

module adt_hashmap__
  use adt_hashmap
  use adt_string
  use adt_list
  use adt_item
  use adt_ref
  use adt_crc

  type(List_t), target :: hashmap_nodeCache

  type, public :: HashNode_t
    type(String_t) :: key
    type(Item_t)   :: value
  end type

  !_TypeGen_declare_RefType( public, HashNode, type(HashNode_t), scalar, \
  !     deleteProc = hashnode_delete, \
  !     cloneMode  = _type )

  !_TypeGen_declare_ListNode( public, HashNode, type(HashNode_t), scalar )

  interface
    subroutine hashnode_delete( self )
      import HashNode_t
      type(HashNode_t), intent(inout) :: self
    end subroutine
  end interface

# define HashMap_t    HashMap_t__impl__

  type, public :: HashMap_t
    type(List_t), dimension(:), pointer :: indexVector    => null()
    integer*4                           :: items          =  0
    integer*4                           :: indexLimits(2) = default_indexLimits
  end type


  interface
    function hashmap_get_bucketIndex( self, key ) result(res)
      import HashMap_t, ListIndex_t
      type(HashMap_t),  intent(in) :: self
      character(len=*), intent(in) :: key
      type(ListIndex_t)            :: res
    end function

    function hashmap_get_value_ref( self, key, clearStale ) result(res)
      import HashMap_t, Item_t
      type(HashMap_t)              :: self
      character(len=*), intent(in) :: key
      logical                      :: clearStale
      type(Item_t),        pointer :: res
    end function

    logical &
    function hashmap_locate_item( self, key, idx )
      import HashMap_t, ListIndex_t
      type(HashMap_t), intent(inout) :: self
      character(len=*),   intent(in) :: key
      type(ListIndex_t), intent(out) :: idx
    end function

    logical &
    function hashmap_unset_( self, key, valTgt )
      import HashMap_t, Item_t
      type(HashMap_t),               intent(inout) :: self
      character(len=*),                 intent(in) :: key
      type(Item_t), optional, pointer, intent(out) :: valTgt
    end function

    subroutine hashmap_clear( self, tgtList )
      import HashMap_t, List_t
      type(HashMap_t), intent(inout) :: self
      type(List_t),         optional :: tgtList
    end subroutine

    subroutine hashmap_setup_index( self, indexSize, tgtList )
      import HashMap_t, List_t
      type(HashMap_t), intent(inout) :: self
      integer*4,          intent(in) :: indexSize
      type(List_t),         optional :: tgtList
    end subroutine

    subroutine hashmap_reindex( self )
      import HashMap_t
      type(HashMap_t), intent(inout) :: self
    end subroutine
  end interface

contains

  !_TypeGen_implementAll()

end module

!_PROC_EXPORT(hashmap_len)
  pure &
  function hashmap_len( self ) result(res)
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(in) :: self
    integer*4                   :: res
    res = self%items
  end function


  subroutine hashnode_delete( self )
    use adt_hashmap__, only: HashNode_t, delete
    implicit none
    type(HashNode_t), intent(inout) :: self
    
    call delete( self%key )
    call delete( self%value )
  end subroutine


!_PROC_EXPORT(hashmap_init_by_proto)
  subroutine hashmap_init_by_proto( self, has_proto, proto )
    use adt_hashmap__; implicit none
    type(HashMap_t)     :: self
    integer             :: has_proto
    type(HashMap_t)     :: proto
    integer*4           :: indexLimits(2)

    self%indexVector => null()
    self%items       =  0
    if (has_proto /= 0) then; indexLimits = proto%indexLimits
                        else; indexLimits = default_indexLimits
    end if
    call hashmap_init_sized( self, indexLimits(1), indexLimits(2) )
  end subroutine


!_PROC_EXPORT(hashmap_init)
  subroutine hashmap_init( self )
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self
    call hashmap_init_sized( self, default_indexLimits(1), default_indexLimits(2) )
  end subroutine


!_PROC_EXPORT(hashmap_init_sized)
  subroutine hashmap_init_sized( self, index_min, index_max )
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self
    integer                        :: index_min, index_max

    self%indexLimits(1) = max(1, index_min)
    self%indexLimits(2) = max(1, index_min, index_max)
    if (.not. is_valid(hashmap_nodeCache)) &
      call initialize( hashmap_nodeCache )
    call hashmap_setup_index( self, self%indexLimits(1) )
  end subroutine


  subroutine hashmap_setup_index( self, indexSize, tgtList )
    use adt_hashmap__, only: HashMap_t, List_t, hashmap_clear, initialize
    implicit none
    type(HashMap_t), intent(inout) :: self
    integer*4,          intent(in) :: indexSize
    type(List_t),         optional :: tgtList
    integer*4                      :: i

    call hashmap_clear( self, tgtList )
    if (indexSize /= size(self%indexVector)) then
      if (associated( self%indexVector )) &
        deallocate( self%indexVector )

      allocate( self%indexVector(0 : indexSize - 1) )
      !DEC$ parallel
      do i = 0, indexSize - 1
        call initialize( self%indexVector(i) )
      end do
    end if
  end subroutine

  
!_PROC_EXPORT(hashmap_clear)
  subroutine hashmap_clear( self, tgtList )
    use adt_hashmap__, only: HashMap_t, List_t, hashmap_nodeCache, append
    implicit none
    type(HashMap_t), intent(inout) :: self
    type(List_t), optional, target :: tgtList
    type(List_t),          pointer :: tgt
    integer*4                      :: i
    
    if (associated( self%indexVector )) then
      if (present(tgtList)) then; tgt => tgtList
                            else; tgt => hashmap_nodeCache
      end if

      do i = 0, size( self%indexVector ) - 1
        call append( tgt, self%indexVector(i) )
      end do
      self%items = 0
    end if
  end subroutine

!_PROC_EXPORT(hashmap_delete)
  subroutine hashmap_delete( self )
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self

    if (associated( self%indexVector )) then
      call hashmap_clear( self )
      deallocate( self%indexVector )
    end if
  end subroutine


!_PROC_EXPORT(hashmap_assign_hashmap)
  subroutine hashmap_assign_hashmap( lhs, rhs )
    use adt_hashmap__; implicit none
    type(HashMap_t), target, intent(inout) :: lhs
    type(HashMap_t), target,    intent(in) :: rhs
    type(ListIndex_t)                      :: l_idx, r_idx
    type(HashNode_t),              pointer :: l_node, r_node
    integer*4                              :: i
   
    if (.not. associated( lhs%indexVector, rhs%indexVector )) then
      lhs%indexLimits = rhs%indexLimits
      call hashmap_pre_cache( rhs%items )
      call hashmap_setup_index( lhs, size(rhs%indexVector) )
      do i = 0, size( lhs%indexVector ) - 1
        call insert( index( lhs%indexVector(i), tail, 0 ), index( hashmap_nodeCache, -len( rhs%indexVector(i) ) ) )
        l_idx = index( lhs%indexVector(i) )
        r_idx = index( rhs%indexVector(i) )
        do while (is_valid(r_idx))
          l_node => HashNode(l_idx)
          r_node => HashNode(r_idx)
          call assign( l_node%key, r_node%key )
          call assign( l_node%value, r_node%value )
          call next( l_idx )
          call next( r_idx )
        end do
      end do
    end if
  end subroutine  


!_PROC_EXPORT(hashmap_clear_cache)
  subroutine hashmap_clear_cache()
    use adt_hashmap__, only: clear, hashmap_nodeCache
    implicit none
    call clear( hashmap_nodeCache )
  end subroutine


  function hashmap_get_bucketIndex( self, key ) result(res)
    use adt_hashmap__, only: HashMap_t, ListIndex_t, crc32, index
    implicit none
    type(HashMap_t),  intent(in) :: self
    character(len=*), intent(in) :: key
    type(ListIndex_t)            :: res
    integer                      :: idx, n
    
    n = size( self%indexVector )
    idx = mod( crc32(key), n )
    ! have to fix negativ indices since fortran doesn't know unsigned integers >:(
    if (idx < 0) &
      idx = idx + n
    res = index( self%indexVector(idx) )
  end function


  logical &
  function hashmap_locate_item( self, key, idx ) result(res)
    use adt_hashmap__, only: HashMap_t, HashNode_t, ListIndex_t, hashmap_reindex, &
                             hashmap_get_bucketIndex, HashNode, next, is_valid
    use adt_string
    implicit none
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    type(ListIndex_t), intent(out) :: idx
    type(HashNode_t),      pointer :: node
    
    if (self%indexLimits(1) < self%indexLimits(2)) &
      call hashmap_reindex( self )
    
    idx = hashmap_get_bucketIndex( self, key )
    do while (is_valid(idx))
      node => HashNode(idx)
      if (node%key /= key) then; call next(idx)
                           else; exit
      end if
    end do
    res = is_valid(idx)
  end function


!_PROC_EXPORT(hashmap_set)
  subroutine hashmap_set( self, key, val )
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    type(Item_t),       intent(in) :: val
    type(Item_t),          pointer :: valPtr

    valPtr => hashmap_get_value_ref( self, key, .false. )
    call assign( valPtr, val )
  end subroutine


!_PROC_EXPORT(hashmap_get)
  function hashmap_get( self, key ) result(res)
    use adt_hashmap__; implicit none
    type(HashMap_t)              :: self
    character(len=*), intent(in) :: key
    type(Item_t),        pointer :: res
    res => hashmap_get_value_ref( self, key, .true. )
  end function


!_PROC_EXPORT(hashmap_get_value_ref)
  function hashmap_get_value_ref( self, key, clearStale ) result(res)
    use adt_hashmap__, only: HashMap_t, Item_t, HashNode_t, ListIndex_t, HashNode, hashmap_nodeCache, &
                             hashmap_locate_item, first, new_ListNode, insert, delete, pop, is_valid, assign
    implicit none
    type(HashMap_t)              :: self
    character(len=*), intent(in) :: key
    logical                      :: clearStale
    type(Item_t),        pointer :: res
    type(HashNode_t),    pointer :: mapItem
    type(ListIndex_t)            :: bucket, idx
  
    if (hashmap_locate_item( self, key, bucket )) then
      ! hash node exists ...
      mapItem => HashNode(bucket)
    else
      ! need to add new hash node
      idx = pop( hashmap_nodeCache, first )
      if (is_valid(idx)) then
        mapItem => HashNode(idx)
        call insert( bucket, idx )
        if (clearStale) &
          call delete( mapItem%value )
      else
        call insert( bucket, new_ListNode( mapItem ) )
      end if
      call assign( mapItem%key, key )
      self%items = self%items + 1
    end if
    res => mapItem%value
  end function


!_PROC_EXPORT(hashmap_get_value_ptr)
  function hashmap_get_value_ptr( self, key ) result(res)
    use adt_hashmap__; implicit none
    type(HashMap_t)              :: self
    character(len=*), intent(in) :: key
    type(Item_t),        pointer :: res
    type(HashNode_t),    pointer :: mapItem
    type(ListIndex_t)            :: bucket
  
    if (hashmap_locate_item( self, key, bucket )) then
      mapItem => HashNode( bucket )
      res => mapItem%value
    else
      res => null()
    end if
  end function


  subroutine hashmap_reindex( self )
    use adt_hashmap__, only: HashMap_t
    implicit none
    type(HashMap_t), intent(inout) :: self
  end subroutine

  
  subroutine hashmap_pre_cache( numItems )
    use adt_hashmap__; implicit none
    integer*4                 :: numItems, missing
    type(HashNode_t), pointer :: mapItem
    missing = numItems - len( hashmap_nodeCache )
    do while (missing > 0)
      call append( hashmap_nodeCache, new_ListNode( mapItem ) )
      missing = missing - 1
    end do
  end subroutine


!_PROC_EXPORT(hashmap_remove_key)
  subroutine hashmap_remove_key( self, key )
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    logical                        :: ignored
    ignored = hashmap_unset_( self, key )
  end subroutine

  
!_PROC_EXPORT(hashmap_unset_key)
  logical &
  function hashmap_unset_key( self, key ) result(res)
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    res = hashmap_unset_( self, key )
  end function


!_PROC_EXPORT(hashmap_pop_key)
  function hashmap_pop_key( self, key ) result(res)
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    type(Item_t),          pointer :: res
    if (.not. hashmap_unset_( self, key, res )) &
      res => null()
  end function


  logical &
  function hashmap_unset_( self, key, valTgt ) result(res)
    use adt_hashmap__, only: HashMap_t, Item_t, HashNode_t, ListIndex_t, &
                             hashmap_locate_item, hashmap_nodeCache, index, append, HashNode
    implicit none
    type(HashMap_t),               intent(inout) :: self
    character(len=*),                 intent(in) :: key
    type(Item_t), optional, pointer, intent(out) :: valTgt
    type(HashNode_t),       pointer              :: mapItem
    type(ListIndex_t)                            :: bucket

    res = hashmap_locate_item( self, key, bucket )
    if (res) then
      call append( hashmap_nodeCache, index( bucket, 0 ) )
      if (present(valTgt)) then
        mapItem => HashNode(bucket)
        valTgt  => mapItem%value
      end if
    end if
  end function


!_PROC_EXPORT(hashmap_set_default)
  function hashmap_set_default( self, key, defaultVal ) result(res)
    use adt_hashmap__; implicit none
    type(HashMap_t),     intent(inout) :: self
    character(len=*),       intent(in) :: key
    type(Item_t), optional, intent(in) :: defaultVal
    type(Item_t),              pointer :: res

    res => hashmap_get_value_ref( self, key, .true. )
    if (present(defaultVal)) then
      if (.not. is_valid(res)) then; call assign( res, defaultVal )
                               else; call delete( defaultVal )
      end if
    end if
  end function


!_PROC_EXPORT(hashmap_has_key)
  logical &
  function hashmap_has_key( self, key ) result(res)
    use adt_hashmap__; implicit none
    type(HashMap_t)              :: self
    character(len=*), intent(in) :: key
    type(ListIndex_t)            :: bucket
    res = hashmap_locate_item( self, key, bucket )
  end function

