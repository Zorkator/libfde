
#include "adt/itfUtil.fpp"

module adt_hashmap__
  use adt_hashmap
  use adt_string
  use adt_list
  use adt_item
  use adt_ref
  use adt_crc
  use iso_c_binding

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
    integer(kind=c_size_t)              :: items          =  0, resize_cnt = 0
    integer(kind=c_size_t)              :: indexLimits(2) = default_indexLimits
  end type


  interface
    function hashmap_get_bucketIndex_( self, key ) result(res)
      import HashMap_t, ListIndex_t
      type(HashMap_t),  intent(in) :: self
      character(len=*), intent(in) :: key
      type(ListIndex_t)            :: res
    end function

    function hashmap_get_value_ref_( self, key, clearStale ) result(res)
      import HashMap_t, Item_t
      type(HashMap_t)              :: self
      character(len=*), intent(in) :: key
      logical                      :: clearStale
      type(Item_t),        pointer :: res
    end function

    logical &
    function hashmap_locate_item_( self, key, idx )
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

    subroutine hashmap_flush_( self, tgtList, delIndex )
      import HashMap_t, List_t
      type(HashMap_t), intent(inout) :: self
      type(List_t)                   :: tgtList
      logical                        :: delIndex
    end subroutine

    subroutine hashmap_setup_index_( self, indexSize, tgtList )
      import HashMap_t, List_t
      type(HashMap_t), intent(inout) :: self
      integer(kind=4),    intent(in) :: indexSize
      type(List_t)                   :: tgtList
    end subroutine

    function hashmap_get_ptr( self, key ) result(res)
      import HashMap_t, Item_t
      type(HashMap_t)              :: self
      character(len=*), intent(in) :: key
      type(Item_t),        pointer :: res
    end function

    function hashmap_set_default( self, key, defaultVal ) result(res)
      import HashMap_t, Item_t
      type(HashMap_t), intent(inout) :: self
      character(len=*),   intent(in) :: key
      type(Item_t)                   :: defaultVal
      type(Item_t),          pointer :: res
    end function

    subroutine hashmap_reindex_( self )
      import HashMap_t
      type(HashMap_t), intent(inout) :: self
    end subroutine
  end interface

contains

  !_TypeGen_implementAll()

end module


!_PROC_EXPORT(hashmap_object_size_c)
  integer(kind=4) &
  function hashmap_object_size_c() result(res)
    use adt_hashmap__; implicit none
    type (HashMap_t) :: tmp
    res = storage_size(tmp) / 8
  end function


!_PROC_EXPORT(hashmap_len_c)
  pure &
  function hashmap_len_c( self ) result(res)
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(in) :: self
    integer(kind=4)             :: res
    res = self%items
  end function


  subroutine hashnode_delete( self )
    use adt_hashmap__, only: HashNode_t, delete
    implicit none
    type(HashNode_t), intent(inout) :: self
    
    call delete( self%key )
    call delete( self%value )
  end subroutine


!_PROC_EXPORT(hashmap_init_by_proto_c)
  subroutine hashmap_init_by_proto_c( self, has_proto, proto )
    use adt_hashmap__; implicit none
    type(HashMap_t)     :: self
    integer             :: has_proto
    type(HashMap_t)     :: proto
    integer(kind=4)     :: indexLimits(2)

    self%indexVector => null()
    self%items       =  0
    if (has_proto /= 0) then; indexLimits = proto%indexLimits
                        else; indexLimits = default_indexLimits
    end if
    call hashmap_init_sized_c( self, indexLimits(1), indexLimits(2) )
  end subroutine


!_PROC_EXPORT(hashmap_init_c)
  subroutine hashmap_init_c( self )
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self
    call hashmap_init_sized_c( self, default_indexLimits(1), default_indexLimits(2) )
  end subroutine


!_PROC_EXPORT(hashmap_init_sized_c)
  subroutine hashmap_init_sized_c( self, index_min, index_max )
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self
    integer(kind=4)                :: index_min, index_max

    self%indexLimits(1) = max(1, index_min)
    self%indexLimits(2) = max(1, index_min, index_max)
    if (.not. is_valid(hashmap_nodeCache)) &
      call initialize( hashmap_nodeCache )
    call hashmap_setup_index_( self, self%indexLimits(1), hashmap_nodeCache )
  end subroutine


  subroutine hashmap_setup_index_( self, indexSize, tgtList )
    use adt_hashmap__, only: HashMap_t, List_t, hashmap_flush_, initialize
    implicit none
    type(HashMap_t), intent(inout) :: self
    integer(kind=4),    intent(in) :: indexSize
    type(List_t)                   :: tgtList
    integer(kind=4)                :: i, cur_size

    ! need to check associated before asking for the size!
    ! gfortran doesn't clear a pointer's size on deallocate - so we might get an
    !   old size even for a non-associated pointer!!!
    if (associated( self%indexVector )) then; cur_size = size(self%indexVector)
                                        else; cur_size = 0
    end if

    if (indexSize /= cur_size) then
      call hashmap_flush_( self, tgtList, .true. )
      allocate( self%indexVector(0 : indexSize - 1) )
      !DEC$ parallel
      do i = 0, indexSize - 1
        call initialize( self%indexVector(i) )
      end do
    end if
  end subroutine

  
  subroutine hashmap_flush_( self, tgtList, delIndex )
    use adt_hashmap__, only: HashMap_t, List_t, append
    implicit none
    type(HashMap_t), intent(inout) :: self
    type(List_t),           target :: tgtList
    logical                        :: delIndex
    type(List_t),          pointer :: tgt
    integer(kind=4)                :: i
    
    if (associated( self%indexVector )) then
      do i = 0, size( self%indexVector ) - 1
        call append( tgtList, self%indexVector(i) )
      end do
      self%items = 0
      if (delIndex) &
        deallocate( self%indexVector )
    end if
  end subroutine


!_PROC_EXPORT(hashmap_clear_c)
  subroutine hashmap_clear_c( self )
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self
    call hashmap_flush_( self, hashmap_nodeCache, .false. )
  end subroutine


!_PROC_EXPORT(hashmap_delete_c)
  subroutine hashmap_delete_c( self )
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self
    call hashmap_flush_( self, hashmap_nodeCache, .true. )
  end subroutine


!_PROC_EXPORT(hashmap_assign_hashmap_c)
  subroutine hashmap_assign_hashmap_c( lhs, rhs )
    use adt_hashmap__; implicit none
    type(HashMap_t), target, intent(inout) :: lhs
    type(HashMap_t), target,    intent(in) :: rhs
    type(ListIndex_t)                      :: l_idx, r_idx
    type(HashNode_t),              pointer :: l_node, r_node
    integer(kind=4)                        :: i
   
    if (.not. associated( lhs%indexVector, rhs%indexVector )) then
      lhs%indexLimits = rhs%indexLimits
      call hashmap_pre_cache( rhs%items )
      call hashmap_setup_index_( lhs, size(rhs%indexVector), hashmap_nodeCache )
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


!_PROC_EXPORT(hashmap_clear_cache_c)
  subroutine hashmap_clear_cache_c()
    use adt_hashmap__, only: clear, hashmap_nodeCache
    implicit none
    call clear( hashmap_nodeCache )
  end subroutine


  function hashmap_get_bucketIndex_( self, key ) result(res)
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
    res = index( self%indexVector(idx), 1, stride=1 )
  end function


  logical &
  function hashmap_locate_item_( self, key, idx ) result(res)
    use adt_hashmap__, only: HashMap_t, HashNode_t, ListIndex_t, hashmap_reindex_, &
                             hashmap_get_bucketIndex_, HashNode, next, is_valid
    use adt_string
    implicit none
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    type(ListIndex_t), intent(out) :: idx
    type(HashNode_t),      pointer :: node
    
    if (self%indexLimits(1) < self%indexLimits(2)) &
      call hashmap_reindex_( self ) !< range in indexLimits enables index-auto-resizing
    
    idx = hashmap_get_bucketIndex_( self, key )
    do while (is_valid(idx))
      node => HashNode(idx)
      if (node%key /= key) then; call next(idx)
                           else; exit
      end if
    end do
    res = is_valid(idx)
  end function


!_PROC_EXPORT(hashmap_set_c)
  subroutine hashmap_set_c( self, key, val )
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    type(Item_t),       intent(in) :: val
    type(Item_t),          pointer :: valPtr

    valPtr => hashmap_get_value_ref_( self, key, .false. )
    call assign( valPtr, val )
  end subroutine


!_PROC_EXPORT(hashmap_get)
  function hashmap_get( self, key ) result(res)
    use adt_hashmap__; implicit none
    type(HashMap_t)              :: self
    character(len=*), intent(in) :: key
    type(Item_t),        pointer :: res
    res => hashmap_get_value_ref_( self, key, .true. )
  end function


!_PROC_EXPORT(hashmap_get_c)
  subroutine hashmap_get_c( res, self, key )
    use adt_hashmap__; implicit none
    type(c_ptr),      intent(inout) :: res
    type(HashMap_t)                 :: self
    character(len=*), intent(in)    :: key
    res = c_loc( hashmap_get_value_ref_( self, key, .true. ) )
  end subroutine


  function hashmap_get_value_ref_( self, key, clearStale ) result(res)
    use adt_hashmap__, only: HashMap_t, Item_t, HashNode_t, ListIndex_t, HashNode, hashmap_nodeCache, &
                             hashmap_locate_item_, first, new_ListNode, insert, delete, pop, is_valid, assign
    implicit none
    type(HashMap_t)              :: self
    character(len=*), intent(in) :: key
    logical                      :: clearStale
    type(Item_t),        pointer :: res
    type(HashNode_t),    pointer :: mapItem
    type(ListIndex_t)            :: bucket, idx
  
    if (hashmap_locate_item_( self, key, bucket )) then
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


!_PROC_EXPORT(hashmap_get_ptr)
  function hashmap_get_ptr( self, key ) result(res)
    use adt_hashmap__, only: HashMap_t, Item_t, HashNode_t, ListIndex_t, hashmap_locate_item_, HashNode
    implicit none
    type(HashMap_t)              :: self
    character(len=*), intent(in) :: key
    type(Item_t),        pointer :: res
    type(HashNode_t),    pointer :: mapItem
    type(ListIndex_t)            :: bucket
  
    if (hashmap_locate_item_( self, key, bucket )) then
      mapItem => HashNode( bucket )
      res => mapItem%value
    else
      res => null()
    end if
  end function


!_PROC_EXPORT(hashmap_get_ptr_c)
  subroutine hashmap_get_ptr_c( res, self, key )
    use adt_hashmap__; implicit none
    type(c_ptr),      intent(inout) :: res
    type(HashMap_t)                 :: self
    character(len=*), intent(in)    :: key
    type(Item_t),           pointer :: ptr

    ptr => hashmap_get_ptr( self, key )
    if (associated(ptr)) then; res = c_loc(ptr)
                         else; res = C_NULL_PTR
    end if
  end subroutine


  subroutine hashmap_reindex_( self )
    use adt_hashmap__, only: HashMap_t, HashNode_t, HashNode, hashmap_setup_index_, hashmap_get_bucketIndex_
    use adt_string
    use adt_list
    use iso_c_binding
    implicit none
    type(HashMap_t), intent(inout) :: self
    type(List_t)                   :: bufList
    type(ListIndex_t)              :: bucket, idx
    type(HashNode_t),      pointer :: node
    integer(kind=c_size_t)         :: cur_size, new_size, items
    real                           :: fract

    cur_size = size( self%indexVector )
    fract    = real(self%items - cur_size) / cur_size
    
    if (abs(fract) > 0.8) then
      new_size = ishft( cur_size, merge( 1, -1, fract > 0 ) )
      new_size = max( new_size, self%indexLimits(1) )
      new_size = min( new_size, self%indexLimits(2) )

      if (new_size /= cur_size) then
        items    = self%items
        call initialize( bufList )
        call hashmap_setup_index_( self, new_size, bufList )
      
        do while (.not. is_empty( bufList ))
          idx    =  index( bufList, first, 0 )
          node   => HashNode( idx )
          bucket =  hashmap_get_bucketIndex_( self, str(node%key) )
          call insert( bucket, idx )
        end do
        self%items = items
        self%resize_cnt = self%resize_cnt + 1
      end if
    end if
  end subroutine


!_PROC_EXPORT(hashmap_get_stats_c)
  subroutine hashmap_get_stats_c( self, stats )
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(in) :: self
    integer(kind=c_size_t)      :: stats(6) ! 1=slots, 2=used, 3=items, 4=minLen, 5=maxLen, 6=resizeCnt
    integer(kind=c_size_t)      :: i, cnt
    stats    = 0;
    stats(1) = size(self%indexVector)
    stats(4) = self%items
    stats(6) = self%resize_cnt
    do i = 0, size( self%indexVector ) - 1
      cnt = len(self%indexVector(i))
      stats(2) = stats(2) + merge( 1, 0, cnt > 0 )
      if (cnt < stats(4)) &
        stats(4) = cnt
      if (cnt > stats(5)) &
        stats(5) = cnt
      stats(3) = stats(3) + cnt
    end do
  end subroutine

  
  subroutine hashmap_pre_cache( numItems )
    use adt_hashmap__; implicit none
    integer(kind=4)           :: numItems, missing
    type(HashNode_t), pointer :: mapItem
    missing = numItems - len( hashmap_nodeCache )
    do while (missing > 0)
      call append( hashmap_nodeCache, new_ListNode( mapItem ) )
      missing = missing - 1
    end do
  end subroutine


!_PROC_EXPORT(hashmap_remove_key_c)
  subroutine hashmap_remove_key_c( self, key )
    use adt_hashmap__; implicit none
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    logical                        :: ignored
    ignored = hashmap_unset_( self, key )
  end subroutine

  
!_PROC_EXPORT(hashmap_unset_key_c)
  logical &
  function hashmap_unset_key_c( self, key ) result(res)
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


!_PROC_EXPORT(hashmap_pop_key_c)
  subroutine hashmap_pop_key_c( res, self, key )
    use adt_hashmap__; implicit none
    type(c_ptr),      intent(inout) :: res
    type(HashMap_t),  intent(inout) :: self
    character(len=*), intent(in)    :: key
    type(Item_t),           pointer :: ptr

    if (hashmap_unset_( self, key, ptr )) then; res = c_loc(ptr)
                                          else; res = C_NULL_PTR
    end if
  end subroutine


  logical &
  function hashmap_unset_( self, key, valTgt ) result(res)
    use adt_hashmap__, only: HashMap_t, Item_t, HashNode_t, ListIndex_t, &
                             hashmap_locate_item_, hashmap_nodeCache, index, append, HashNode
    implicit none
    type(HashMap_t),               intent(inout) :: self
    character(len=*),                 intent(in) :: key
    type(Item_t), optional, pointer, intent(out) :: valTgt
    type(HashNode_t),       pointer              :: mapItem
    type(ListIndex_t)                            :: bucket

    res = hashmap_locate_item_( self, key, bucket )
    if (res) then
      call append( hashmap_nodeCache, index( bucket, 0 ) )
      self%items = self%items - 1
      if (present(valTgt)) then
        mapItem => HashNode(bucket)
        valTgt  => mapItem%value
      end if
    end if
  end function


!_PROC_EXPORT(hashmap_set_default)
  function hashmap_set_default( self, key, defaultVal ) result(res)
    use adt_hashmap__, only: HashMap_t, Item_t, hashmap_get_value_ref_, is_valid, assign, delete
    implicit none
    type(HashMap_t),     intent(inout) :: self
    character(len=*),       intent(in) :: key
    type(Item_t)                       :: defaultVal
    type(Item_t),              pointer :: res

    res => hashmap_get_value_ref_( self, key, .true. )
    if (.not. is_valid(res)) then; call assign( res, defaultVal ) !< key didn't exist before
                             else; call delete( defaultVal )      !< key existed - so delete given defaultVal
    end if
  end function


!_PROC_EXPORT(hashmap_set_default_c)
  subroutine hashmap_set_default_c( res, self, key, defaultVal )
    use adt_hashmap__; implicit none
    type(c_ptr),         intent(inout) :: res
    type(HashMap_t),     intent(inout) :: self
    character(len=*),       intent(in) :: key
    type(Item_t)                       :: defaultVal
    res = c_loc( hashmap_set_default( self, key, defaultVal ) )
  end subroutine


!_PROC_EXPORT(hashmap_has_key_c)
  logical &
  function hashmap_has_key_c( self, key ) result(res)
    use adt_hashmap__; implicit none
    type(HashMap_t)              :: self
    character(len=*), intent(in) :: key
    type(ListIndex_t)            :: bucket
    res = hashmap_locate_item_( self, key, bucket )
  end function

