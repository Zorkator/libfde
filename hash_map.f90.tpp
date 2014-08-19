
module hash_map
  use abstract_list
  use var_item
  use dynamic_string
  use generic_ref
  implicit none

  integer, parameter :: min_indexSize = 10
  integer, parameter :: max_indexSize = 100000


  type, public :: HashMapItem_t
    private
    type(DynamicString_t) :: key
    type(VarItem_t)       :: value
  end type


  type, public :: HashMap_t
    type(List_t), dimension(:), pointer :: indexVector    => null()
    integer*4                           :: items          =  0
    integer*4                           :: indexLimits(2) = (/ min_indexSize, max_indexSize /)
  end type

  
  type(List_t), target :: hm_nodeCache


  !_TypeGen_declare_RefType( private, mapItem, type(HashMapItem_t), scalar )
  !_TypeGen_declare_ListItem( private, mapItem, type(HashMapItem_t), scalar )

contains

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
    type(HashMapItem_t)            :: item

    self%indexLimits = (/ index_min, index_max /)
    if (.not. is_valid(hm_nodeCache)) &
      call initialize( hm_nodeCache, item_type(item%value) )
    call hm_setup_index( self, self%indexLimits(1) )
  end subroutine


  subroutine hm_setup_index( self, indexSize, tgtList )
    type(HashMap_t), intent(inout) :: self
    integer*4,          intent(in) :: indexSize
    type(List_t),         optional :: tgtList

    call hm_clear( self, tgtList )
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

  !_TypeGen_implementAll()

end

