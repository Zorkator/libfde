
! helper macros for extending abstract_list
! to be provided by #include "adt/abstract_list.fpp"
! OR
!  extend refgen.py ?

# define _implementNewItem_( typeId, baseType ) \
    ...
# define _implementItemCast_( typeId, baseType ) \
    ...

module mylist
  use abstract_list
  implicit none

  type :: MyItem_t
    type (Item_t) :: super
    integer*4     :: value
  end type

  !interface MyItem;  module procedure myitem_downcast; end interface
  interface MyValue; module procedure myitem_value;    end interface

  !public :: MyItem
  public :: MyValue

contains

  function newItem( val ) result(res)
    integer*4,    intent(in) :: val
    type (Item_t),   pointer :: res
    type (MyItem_t), pointer :: node
    allocate( node )
    node%value = val
    res => node%super
  end function

  !function myitem_downcast( node ) result(res)
  !  use iso_c_binding
  !  type (Item_t),    target :: node
  !  type (MyItem_t), pointer :: res
  !  call c_f_pointer( c_loc(node), res )
  !end function

  function myitem_value( node ) result(res)
    use iso_c_binding
    type (Item_t),    target :: node
    integer*4,       pointer :: res
    type (MyItem_t), pointer :: ptr
    call c_f_pointer( c_loc(node), ptr )
    res => ptr%value
  end function

end module


program testinger
  use var_item
  use abstract_list
  use base_types
  use mylist
  use iso_c_binding
  implicit none

  type (List_t) :: l
  integer*4   :: cnt
  type (MyItem_t), pointer :: ptr
  type (VarItem_t)         :: var
  type (GenericRef_t)      :: ref1
  type (ListIterator_t)    :: itr
  procedure(), pointer :: castProc => null()

  call initialize( l, static_type(1) )

  print *, is_valid(l)
  print *, is_valid(l, static_type(1))
  print *, is_valid(l, static_type(2.3))

  itr = inserter( l )
  itr = inserter( l, front(l) )
  itr = inserter( l, back(l) )

  do cnt = 1, 10
    call append( l, newItem( cnt ) )
  end do

  itr = iterator( l, front(l) )
  do while (is_valid(itr))
    print *, MyValue( itr%node )
    itr = next(itr)
  end do

  itr = iterator( l, back(l), -2 )
  do while (is_valid(itr))
    print *, MyValue( itr%node )
    itr = next(itr)
  end do

  itr = iterator( l, 3 )
  do while (is_valid(itr))
    print *, MyValue( itr%node )
    itr = next(itr)
  end do

  itr = iterator( l, front(l) )
  itr = iterator( l, front(l), 2 )
  itr = iterator( l, front(l), -1 )
  itr = iterator( l, back(l) )
  itr = iterator( l, back(l), -1 )
  itr = iterator( l, back(l), 1 )
  itr = iterator( l, itr, 1 )
  itr = iterator( l, itr, 1 )

  ref1 = ref(l)

  print *, len(List(ref1))

  print *, MyValue( front(l) )

  !call cast( cfront(l), ptr )

  !ref1 = clone(ref1)
  !call free( ref1 ) !< segfaults because of shallow copy!

  call clear(l)
  call delete( l )


  
  contains

  !subroutine cast( c, f )
  !  type(c_ptr),              intent(in) :: c
  !  type(MyItem_t), pointer, intent(out) :: f
  !  call c_f_pointer( c, f )
  !end subroutine


end program

