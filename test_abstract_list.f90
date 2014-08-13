
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

  interface value; module procedure myitem_value;    end interface
  public :: value

contains

  function newItem( val ) result(res)
    integer*4,    intent(in) :: val
    type (Item_t),   pointer :: res
    type (MyItem_t), pointer :: node
    allocate( node )
    node%value = val
    res => node%super
  end function

  function myitem_value( idx ) result(res)
    use iso_c_binding
    type(ListIndex_t)    :: idx
    integer*4,      pointer :: res
    type(MyItem_t), pointer :: ptr
    call c_f_pointer( c_loc(idx%node), ptr )
    res => ptr%value
  end function

end module


#define _exp(what)    print *, "exp: " // what

program testinger
  use var_item
  use abstract_list
  use base_types
  use mylist
  use iso_c_binding
  implicit none

  type (List_t) :: l1, l2
  integer*4   :: cnt
  type (MyItem_t), pointer :: ptr
  type (VarItem_t)         :: var
  type (GenericRef_t)      :: ref1
  type (ListIndex_t)       :: idx
  procedure(), pointer :: castProc => null()
  integer*4                :: array(10)

  call initialize( l1, static_type(1) )
  call initialize( l2, static_type(1) )

  print *, is_valid(l1)
  print *, is_valid(l1, static_type(1))
  print *, is_valid(l1, static_type(2.3))

  do cnt = 1, 10
    array(cnt) = cnt
    call append( l2, newItem( cnt ) )
  end do

  _exp("empty")
  call printItems( index( l1 ) )
  _exp("1..10")
  call printItems( index( l2 ) )
  call append( l1, l2 )
  _exp("1..10")
  call printItems( index( l1 ) )
  _exp("empty")
  call printItems( index( l2 ) )

  _exp("1..10")
  call printItems( index(l1, first) )
  _exp("empty")
  call printItems( index(l1, tail) )

  idx = index( l1, 3 )
  do cnt = -5, -1
    call insert( index(l1, last), newItem(cnt) )
  end do

  _exp("1..9, -5..-1, 10")
  call printItems( index( l1, first ) )
  _exp("1")
  call printItems( index( l1, first, -1 ) )
  _exp("10")
  call printItems( index( l1, last ) )
  _exp("10, -1..-5, 9..1")
  call printItems( index( l1, last , -1 ) )
  _exp("empty")
  call printItems( index( l1, tail ) )
  _exp("empty")
  call printItems( index( l1, tail ) )

  _exp("5..9, -5..-1, 10")
  call printItems( index( l1, 5, 1 ) )
  _exp("5..1")
  call printItems( index( l1, 5, -1 ) )
  _exp("-4..-1, 10")
  call printItems( index( l1, -5 ) )
  _exp("6..9, -5..-1, 10")
  call printItems( index( l1, -10 ) )

  _exp("1..9, -5..-1, 10")
  call printRange( index(l1), index(l1, tail) )

  _exp("1..9, -5..-1, 10")
  call printItems( index( l1 ) )

  _exp("10,-2,-4,9,7,5,3,1")
  call printItems( index(l1, last, -2) )

  idx = index( l1, last, -2 )
  _exp("idx /= next(idx): "), idx /= get_next(idx)
  _exp("next(idx) == next(idx): "), get_next(idx) == get_next(idx)

  _exp("empty")
  call printItems( index( l1, tail, -1 ) )
  
  call insert( index(l2),  index(l1, 5), index(l1, last) )
  _exp("1..4")
  call printItems( index(l1) )
  _exp('len 4, 11:'), len(l1), len(l2)
  _exp("5..9, -5..-1, 10")
  call printItems( index(l2) )
  _exp('len 11, 4:'), len(l2), len(l1)

  call fill( l1, 1, 10 )
  call fill( l2, -10, -1 )
  call printList( l1 )
  call printList( l2 )


  call insert( index(l1, first, 0), index(l2, last, -1) )
  call append( l2, index(l1, first, 2) )

  call remove( index(l1, 2, 0) )
  call remove( index(l1, 2, 2) )


  idx = index( l1 )
  idx = index( idx, 2 )
  idx = index( idx, -1 )
  idx = index( l1, last )
  idx = index( l1, last, -1 )
  idx = index( l1, last, 1 )
  idx = index( idx, 1 )
  idx = index( idx, 1 )

  print *, array( 1: )
  print *, array( 1::5 )
  print *, array( ::5 )
  print *, array( ::-2 ) !< leer
  print *, array( :2 )
  print *, array( :-2 )
  print *, array( :-2 )
  
  array(::2) = 0
  print *, array

  ! giving an end makes an index effectively a range
  ! end is not implemented yet ...
  !idx = index( l1, begin, end, stride )

  ref1 = ref(l1)

  print *, len(List(ref1))

  print *, value( index(l1) )

  !ref1 = clone(ref1)
  !call free( ref1 ) !< segfaults because of shallow copy!

  call clear( l1 )
  call delete( l1 )
  call delete( l2, static_type(l2) )

  
  contains

  subroutine printItems( idx )
    type(ListIndex_t) :: idx

    print *, "items:"
    do while (is_valid(idx))
      print *, value(idx)
      call next(idx)
    end do
    print *, "########"
  end subroutine


  subroutine printRange( beg, end )
    type(ListIndex_t) :: beg, end
    
    print *, "items:"
    do while (beg /= end)
      print *, value(beg)
      call next(beg)
    end do
    print *, "########"
  end subroutine

  
  subroutine fill( list, beg, end, stride )
    type(List_t)        :: list
    integer*4, optional :: beg, end, stride
    integer*4           :: beg_, end_, stride_, i

    beg_    = first
    end_    = last
    stride_ = 1
    if (present(beg))    beg_    = beg
    if (present(end))    end_    = end
    if (present(stride)) stride_ = stride

    call delete( list, static_type(1) )
    do i = beg_, end_, stride_
      call append( list, newItem(i) )
    end do
  end subroutine

  
  subroutine printList( list )
    type(List_t)      :: list
    type(ListIndex_t) :: idx

    idx = index(list)
    do while (is_valid(idx))
      write(*,'(I4)',advance="no") value(idx)
      call next(idx)
    end do
    print *, ''
    print *, 'items: ', len(list)
  end subroutine

end program

