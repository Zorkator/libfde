
module hash_map
  use abstract_list
  use var_item
  use dynamic_string
  use generic_ref
  implicit none
  private

  integer, parameter :: min_indexSize = 10
  integer, parameter :: max_indexSize = 100000


  type, private :: HashNode_t
    private
    type(DynamicString_t) :: key
    type(VarItem_t)       :: value
  end type

  !_TypeGen_declare_RefType( private, HashNode, type(HashNode_t), scalar, \
  !     deleteProc = hn_delete, \
  !     cloneMode  = _type )

  !_TypeGen_declare_ListItem( private, HashNode, type(HashNode_t), scalar )


  type, public :: HashMap_t
    type(List_t), dimension(:), pointer :: indexVector    => null()
    integer*4                           :: items          =  0
    integer*4                           :: indexLimits(2) = (/ min_indexSize, max_indexSize /)
  end type

  !_TypeGen_declare_RefType( public, HashMap, type(HashMap_t), scalar, \
  !     initProc   = hm_initialize_proto, \
  !     assignProc = hm_assign_hm,        \
  !     deleteProc = hm_delete,           \
  !     cloneMode  = _type )

  !_TypeGen_declare_ListItem( public, HashMap, type(HashMap_t), scalar )

  
  type(List_t), target :: hm_nodeCache

  
  interface initialize ; module procedure hm_initialize, hm_initialize_default ; end interface
  interface len        ; module procedure hm_len                               ; end interface
  interface clear      ; module procedure hm_clear                             ; end interface
  interface delete     ; module procedure hm_delete                            ; end interface
  interface set        ; module procedure hm_set                               ; end interface
  interface get        ; module procedure hm_get                               ; end interface
  interface remove     ; module procedure hm_remove_key                        ; end interface
  interface unset      ; module procedure hm_unset_key                         ; end interface
  interface pop        ; module procedure hm_pop_key                           ; end interface
  interface setDefault ; module procedure hm_set_default                       ; end interface
  interface hasKey     ; module procedure hm_has_key                           ; end interface

  interface assign        ; module procedure hm_assign_hm                      ; end interface
  interface assignment(=) ; module procedure hm_assign_hm                      ; end interface

  public :: initialize
  public :: clear
  public :: delete
  public :: get, set, remove, unset, pop
  public :: setDefault, hasKey
  public :: hm_clear_cache
  public :: assign, assignment(=)

contains

  !_TypeGen_implementAll()

  pure &
  function hm_len( self ) result(res)
    type(HashMap_t), intent(in) :: self
    integer*4                   :: res
    res = self%items
  end function


  subroutine hn_delete( self )
    type(HashNode_t), intent(inout) :: self
    
    call delete( self%key )
    call delete( self%value )
  end subroutine


  subroutine hm_initialize_proto( self, has_proto, proto )
    type(HashMap_t)     :: self
    integer             :: has_proto
    type(HashMap_t)     :: proto
    integer*4           :: indexLimits(2)

    self%indexVector => null()
    self%items       =  0
    if (has_proto /= 0) then; indexLimits = proto%indexLimits
                        else; indexLimits = (/ min_indexSize, max_indexSize /)
    end if
    call hm_initialize( self, indexLimits(1), indexLimits(2) )
  end subroutine


  subroutine hm_initialize_default( self )
    type(HashMap_t), intent(inout) :: self
    call hm_initialize( self, min_indexSize, max_indexSize )
  end subroutine


  subroutine hm_initialize( self, index_min, index_max )
    type(HashMap_t), intent(inout) :: self
    integer                        :: index_min, index_max

    self%indexLimits(1) = max(1, index_min)
    self%indexLimits(2) = max(1, index_min, index_max)
    if (.not. is_valid(hm_nodeCache)) &
      call initialize( hm_nodeCache )
    call hm_setup_index( self, self%indexLimits(1) )
  end subroutine


  subroutine hm_setup_index( self, indexSize, tgtList )
    type(HashMap_t), intent(inout) :: self
    integer*4,          intent(in) :: indexSize
    type(List_t),         optional :: tgtList
    integer*4                      :: i

    call hm_clear( self, tgtList )
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

  
  subroutine hm_clear( self, tgtList )
    type(HashMap_t), intent(inout) :: self
    type(List_t), optional, target :: tgtList
    type(List_t),          pointer :: tgt
    integer*4                      :: i
    
    if (associated( self%indexVector )) then
      if (present(tgtList)) then; tgt => tgtList
                            else; tgt => hm_nodeCache
      end if

      do i = 0, size( self%indexVector ) - 1
        call append( tgt, self%indexVector(i) )
      end do
      self%items = 0
    end if
  end subroutine


  subroutine hm_delete( self )
    type(HashMap_t), intent(inout) :: self

    if (associated( self%indexVector )) then
      call hm_clear( self )
      deallocate( self%indexVector )
    end if
  end subroutine


  subroutine hm_assign_hm( lhs, rhs )
    type(HashMap_t), target, intent(inout) :: lhs
    type(HashMap_t), target,    intent(in) :: rhs
    type(ListIndex_t)                      :: l_idx, r_idx
    type(HashNode_t),              pointer :: l_node, r_node
    integer*4                              :: i
   
    if (.not. associated( lhs%indexVector, rhs%indexVector )) then
      lhs%indexLimits = rhs%indexLimits
      call hm_pre_cache( rhs%items )
      call hm_setup_index( lhs, size(rhs%indexVector) )
      do i = 0, size( lhs%indexVector ) - 1
        !lhs%indexVector(i) = rhs%indexVector(i) !< too simple ... should reuse cached items
        call insert( index( lhs%indexVector(i), tail, 0 ), index( hm_nodeCache, -len( rhs%indexVector(i) ) ) )
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


  subroutine hm_clear_cache()
    call clear( hm_nodeCache )
  end subroutine


  function hm_get_bucketIndex( self, key ) result(res)
    use adt_crc
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
  function hm_locate_item( self, key, idx ) result(res)
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    type(ListIndex_t), intent(out) :: idx
    type(HashNode_t),      pointer :: item
    
    if (self%indexLimits(1) < self%indexLimits(2)) &
      call hm_reindex( self )
    
    idx = hm_get_bucketIndex( self, key )
    do while (is_valid(idx))
      item => HashNode(idx)
      if (item%key /= key) then; call next(idx)
                           else; exit
      end if
    end do
    res = is_valid(idx)
  end function


  subroutine hm_set( self, key, val )
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    type(VarItem_t),    intent(in) :: val
    type(VarItem_t),       pointer :: valPtr

    valPtr => hm_get_value_ref( self, key, .false. )
    call assign( valPtr, val )
  end subroutine


  function hm_get( self, key ) result(res)
    type(HashMap_t)              :: self
    character(len=*), intent(in) :: key
    type(VarItem_t),     pointer :: res
    res => hm_get_value_ref( self, key, .true. )
  end function


  function hm_get_value_ref( self, key, clearStale ) result(res)
    type(HashMap_t)              :: self
    character(len=*), intent(in) :: key
    logical                      :: clearStale
    type(VarItem_t),     pointer :: res
    type(HashNode_t),    pointer :: mapItem
    type(ListIndex_t)            :: bucket, idx
  
    if (hm_locate_item( self, key, bucket )) then
      ! hash node exists ...
      mapItem => HashNode(bucket)
    else
      ! need to add new hash node
      idx = pop( hm_nodeCache, first )
      if (is_valid(idx)) then
        mapItem => HashNode(idx)
        call insert( bucket, idx )
        if (clearStale) &
          call delete( mapItem%value )
      else
        call insert( bucket, new_ListItem( mapItem ) )
      end if
      call assign( mapItem%key, key )
      self%items = self%items + 1
    end if
    res => mapItem%value
  end function

  
  function hm_get_value_ptr( self, key ) result(res)
    type(HashMap_t)              :: self
    character(len=*), intent(in) :: key
    type(VarItem_t),     pointer :: res
    type(HashNode_t),    pointer :: mapItem
    type(ListIndex_t)            :: bucket
  
    if (hm_locate_item( self, key, bucket )) then
      mapItem => HashNode( bucket )
      res => mapItem%value
    else
      res => null()
    end if
  end function


  subroutine hm_reindex( self )
    type(HashMap_t), intent(inout) :: self
  end subroutine

  
  subroutine hm_pre_cache( numItems )
    integer*4                 :: numItems, missing
    type(HashNode_t), pointer :: mapItem
    missing = numItems - len( hm_nodeCache )
    do while (missing > 0)
      call append( hm_nodeCache, new_ListItem( mapItem ) )
      missing = missing - 1
    end do
  end subroutine


  subroutine hm_remove_key( self, key )
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    logical                        :: ignored
    ignored = hm_unset_( self, key )
  end subroutine

  
  logical &
  function hm_unset_key( self, key ) result(res)
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    res = hm_unset_( self, key )
  end function


  function hm_pop_key( self, key ) result(res)
    type(HashMap_t), intent(inout) :: self
    character(len=*),   intent(in) :: key
    type(VarItem_t),       pointer :: res
    if (.not. hm_unset_( self, key, res )) &
      res => null()
  end function


  logical &
  function hm_unset_( self, key, valTgt ) result(res)
    type(HashMap_t),                  intent(inout) :: self
    character(len=*),                    intent(in) :: key
    type(VarItem_t), optional, pointer, intent(out) :: valTgt
    type(HashNode_t),          pointer              :: mapItem
    type(ListIndex_t)                               :: bucket

    res = hm_locate_item( self, key, bucket )
    if (res) then
      call append( hm_nodeCache, index( bucket, 0 ) )
      if (present(valTgt)) then
        mapItem => HashNode(bucket)
        valTgt  => mapItem%value
      end if
    end if
  end function

  
  function hm_set_default( self, key, defaultVal ) result(res)
    type(HashMap_t),        intent(inout) :: self
    character(len=*),          intent(in) :: key
    type(VarItem_t), optional, intent(in) :: defaultVal
    type(VarItem_t),              pointer :: res

    res => hm_get_value_ref( self, key, .true. )
    if (present(defaultVal)) then
      if (.not. is_valid(res)) then; call assign( res, defaultVal )
                               else; call delete( defaultVal )
      end if
    end if
  end function

  
  logical &
  function hm_has_key( self, key ) result(res)
    type(HashMap_t)              :: self
    character(len=*), intent(in) :: key
    type(ListIndex_t)            :: bucket
    res = hm_locate_item( self, key, bucket )
  end function

end

