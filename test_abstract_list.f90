
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
    type (Item) :: super
    integer*4   :: value
  end type

  interface MyItem;  module procedure myitem_downcast; end interface
  interface MyValue; module procedure myitem_value;    end interface

  public :: MyItem, MyValue

contains

  function newItem( val ) result(res)
    integer*4,    intent(in) :: val
    type (Item),     pointer :: res
    type (MyItem_t), pointer :: node
    allocate( node )
    node%value = val
    res => node%super
  end function

  function myitem_downcast( node ) result(res)
    use iso_c_binding
    type (Item),      target :: node
    type (MyItem_t), pointer :: res
    call c_f_pointer( c_loc(node), res )
  end function

  function myitem_value( node ) result(res)
    use iso_c_binding
    type (Item),      target :: node
    integer*4,       pointer :: res
    type (MyItem_t), pointer :: ptr
    call c_f_pointer( c_loc(node), ptr )
    res => ptr%value
  end function

end module


program testinger
  use var_item
  use abstract_list
  use mylist
  implicit none

  type (List) :: l
  integer*4   :: cnt
  type (MyItem_t), pointer :: ptr

  call al_initialize( l, vi_type_VarItem )

  do cnt = 1, 10
    call al_append_item( l, newItem( cnt ) )
  end do

  print *, MyValue( front(l) )

  call al_delete( l )

end program

