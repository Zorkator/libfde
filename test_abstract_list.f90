
#define _exp(what)    print *, "exp: " // what

program testinger
  use var_item
  use adt_basetypes
  use dynamic_string
  use abstract_list
  use generic_ref
  use iso_c_binding
  implicit none

  type (List_t) :: l1, l2, l3, l4, l5, l6, l7
  integer*4   :: cnt
  type(VarItem_t)         :: var
  type(GenericRef_t)      :: ref1
  type(ListIndex_t)       :: idx
  type(DynamicString_t)   :: strg 
  procedure(), pointer :: castProc => null()

  call initialize( l1 )
  call initialize( l2 )
  call initialize( l3 )
  call initialize( l4 )
  call initialize( l5 )
  call initialize( l6 )
  call initialize( l7 ) 

  var = 42
  call append( l6, new_ListItem_of(var) )
  call append( l6, new_ListItem_of(VarItem_of(42)) )
  call append( l6, new_ListItem_of(VarItem_of('string')) )
  call append( l6, new_ListItem_of(VarItem_of(ref_of(var))) )
  l7 = l6

  call append( l6, new_ListItem_of(VarItem_of(ref_of(var))) )
  call assign( l6, l7 )

  var = "testinger"
  strg = var
  var = VarItem_of(strg)
  var = VarItem_of(ref1)


  print *, is_valid(l1)
  print *, is_valid(l1)
  print *, is_valid(l1)

  do cnt = 1, 10
    call append( l2, new_ListItem_of( cnt ) )
  end do

  call append( l3, new_ListItem_of( l2 ) )
  call append( l3, new_ListItem_of( l2 ) )

  do cnt = 1, 5
    call append( l4, new_ListItem_of(DynamicString('test')) )
    call append( l4, new_ListItem_of('test') )
  end do

  ! list of references ... 
  ref1 = ref_of(cnt)
  do cnt = 1, 3
    call append( l5, new_ListItem_of(ref1) )
    call append( l5, new_ListItem_of(ref_of(cnt)) )
  end do

  idx = index(l5)
  do while (is_valid(idx))
    print *, int32(ref(idx))
    call next(idx)
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
    call insert( index(l1, last), new_ListItem_of(cnt) )
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

  l2 = l1
  call printList( l1 )
  call printList( l2 )

  l2 = index(l1, last, -1)
  call printList( l2 )
  l2 = index(l2, 1, 2 )
  call printList( l2 )


  call insert( index(l1, first, 0), index(l2, last, -1) )
  call append( l2, index(l1, first, 2) )

  call remove( index(l1, 2, 0) )
  call remove( index(l1, 2, 2) )

  call fill( l2, 1, 10 )
  idx = index(l2, last)
  idx = pop( idx )
  print *, real32(idx)

  idx = pop( l2, first )
  print *, real32(idx)
  print *, real32( pop( l2, last ) )

  call clear( l1 )
  call fill( l2, 1, 10 )
  idx = index(l2, first) !< striding index
  print *, is_valid(idx) !< should be valid!
  call insert( index(l1), idx )
  print *, is_valid(idx) !< should be invalid!
  call printList(l1) !< 1-10
  call printList(l2) !< empty

  idx = index(l2, first) !< invalid index
  print *, is_valid(idx) !< should be invalid!
  call insert( index(l1), idx )
  print *, is_valid(idx) !< should be invalid!
  call printList(l1) !< 1-10
  call printList(l2) !< empty
  
  idx = index(l1, first, 0) !< fixed index
  print *, is_valid(idx) !< should be valid!
  call insert( index(l2), idx )
  print *, is_valid(idx) !< should still be valid!
  call printList(l1) !< 2-10
  call printList(l2) !< 1

  call clear( l1 )
  call fill( l2, 1, 10 )
  call insert( index(l1,tail), index(l2,stride=2) )
  call printList(l1) !< 1,3,5,7,9
  call printList(l2) !< 2,4,6,8,10

  call insert( index(l1,stride=1), index(l2) )
  call printList(l1) !< 2,1,4,3,6,5,8,7,10,9
  call printList(l2) !< empty

  idx = pop( l1, first ) !< pop first
  print *, is_valid(idx) !< should be valid!
  call printList(l1) !< 1,4,3,6,5,8,7,10,9
  print *, real32(idx) !< 2.0
  call insert( index(l2, tail), idx )
  print *, is_valid(idx) !< should still be valid!
  call printList(l1) !< 1,4,3,6,5,8,7,10,9
  call printList(l2) !< 2

  call insert( index(l2,tail), pop(l1, first) )
  call printList(l1) !< 4,3,6,5,8,7,10,9
  call printList(l2) !< 2,1


  idx = index( l1 )
  idx = index( idx, 2 )
  idx = index( idx, -1 )
  idx = index( l1, last )
  idx = index( l1, last, -1 )
  idx = index( l1, last, 1 )
  idx = index( idx, 1 )
  idx = index( idx, 1 )

  ! giving an end turns index effectively into a range
  ! end is not implemented yet ...
  !idx = index( l1, begin, end, stride )

  ref1 = ref_of(l1)

  print *, len(List(ref1))

  print *, real32( index(l1) )

  ref1 = clone(ref1)
  call printList( List(ref1) )

  call free( ref1 )
  call delete( ref1 )
  call delete( l1 )
  call delete( l2 )
  call delete( l3 )
  call delete( l4 )
  call delete( l5 )
  call delete( l6 )
  call delete( l7 )
  call delete( strg )

  
  contains


  subroutine printItems( idx )
    type(ListIndex_t) :: idx

    print *, "items:"
    do while (is_valid(idx))
      print *, int32(idx)
      call next(idx)
    end do
    print *, "########"
  end subroutine


  subroutine printRange( beg, end )
    type(ListIndex_t) :: beg, end
    
    print *, "items:"
    do while (beg /= end)
      print *, int32(beg)
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

    call clear( list )
    do i = beg_, end_, stride_
      call append( list, new_ListItem_of(real(i,4)) )
    end do
  end subroutine

  
  subroutine printList( list )
    type(List_t)      :: list
    type(ListIndex_t) :: idx

    idx = index(list)
    do while (is_valid(idx))
      write(*,'(F6.1)',advance="no") real32(idx)
      call next(idx)
    end do
    print *, ''
    print *, 'items: ', len(list)
  end subroutine

end program

