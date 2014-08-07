
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

  interface MyItem;  module procedure myitem_downcast; end interface
  interface MyValue; module procedure myitem_value;    end interface

  public :: MyItem, MyValue

contains

  function newItem( val ) result(res)
    integer*4,    intent(in) :: val
    type (Item_t),   pointer :: res
    type (MyItem_t), pointer :: node
    allocate( node )
    node%value = val
    res => node%super
  end function

  function myitem_downcast( node ) result(res)
    use iso_c_binding
    type (Item_t),    target :: node
    type (MyItem_t), pointer :: res
    call c_f_pointer( c_loc(node), res )
  end function

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
  procedure(), pointer :: castProc => null()

  call initialize( l, static_type(1) )

  do cnt = 1, 10
    call append( l, newItem( cnt ) )
  end do

  ref1 = ref(l)

  print *, MyValue( front(l) )

  call cast( cfront(l), ptr )

  ref1 = clone(ref1)

  call free( ref1 ) !< segfaults because of shallow copy!
  !call delete( l )

  
  contains

  subroutine cast( c, f )
    type(c_ptr),              intent(in) :: c
    type(MyItem_t), pointer, intent(out) :: f
    call c_f_pointer( c, f )
  end subroutine


end program

