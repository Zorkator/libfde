
subroutine startTiming( time_begin )
  real :: time_begin
  call cpu_time( time_begin )
end subroutine


subroutine getTiming( descr, time_begin )
  character(len=*) :: descr
  real             :: time_begin, time_end
  call cpu_time( time_end )
  print*, descr, ': ', (time_end - time_begin), ' seconds'
end subroutine


subroutine test_HashMap( num, idx_min, idx_max )
  use adt_hashmap
  use adt_item
  implicit none

  type (HashMap_t)       :: map
  character(20)          :: key
  integer                :: num, idx_min, idx_max, i, stats(6)
  logical                :: ok
  type (Item_t)          :: val, val2
  type (Item_t), pointer :: valRef => null()
  real                   :: t

  call initialize( map, idx_min, idx_max )

  valRef => get( map, 'test' )
  valRef = 4
  valRef => get( map, 'test' )
  ok = unset( map, 'test' )
  ok = hasKey( map, 'test' )
  valRef => get( map, 'test' )
  valRef = 5

  ! fill map
  call startTiming(t)
  do i = 1, num
    write( key, '(i10)' ) i
    call set( map, key, Item_of(i) )
  end do
  call getTiming('write',t)

  call hashmap_get_stats( map, stats )
  write(*,*) "stats:", stats
  if (stats(3) .ne. len(map)) &
    write(*,*) "item count missmatch! counted: ", stats(3), " vs. registered: ", len(map)

  write( key, '(i10)' ) 42
  ok = unset( map, key )

  ! read back ...
  call startTiming(t)
  do i = 1, num
    write( key, '(i10)' ) i
    val = get( map, key )
    if (is_valid(val)) then
      if (int32(val) == i) &
        cycle
    end if
    print*, trim(key), " = ????"
  end do
  call getTiming('read',t)

  call startTiming(t)
  call delete( map )
  call getTiming('delete',t)
  print*

end subroutine



program testinger
  use adt_hashmap
  use adt_item
  use adt_string
  use adt_ref
  implicit none

  type(HashMap_t)         :: map, map2
  type(Item_t),   pointer :: val
  type(String_t), pointer :: s
  character(len=20)       :: key
  integer                 :: i

  call initialize( map )
  call initialize( map, 10, 100 )
  call initialize( map, -10, 100 )
  call initialize( map, 0, 100 )
  call initialize( map, 0, -1 )
  call initialize( map, 2, 1 )
  call initialize( map2, 2, 1 )

  call set( map, 'bla & text', Item_of('value string') )

  val => get( map, 'testinger' )
  val = 42

  val => get( map, 'bla & text' )
  s => string(val)
  print *, str(s)

  call set( map, 'val-ref', Item_of(ref_of(val)) )

  val => get( map, 'val-ref' )
  s => String(Item(ref(val)))
  print *, str(s)
  val = 'replace ref by text'

  val => setdefault( map, 'bool', Item_of(.true.) )
  val => setdefault( map, 'float', Item_of(4.234) )
  val => setdefault( map, 'string', Item_of('char string') )
  val => setdefault( map, 'float', Item_of('char string') )

  call clear( map )

  call set( map, 'four', Item_of(4) )
  call set( map, 'float', Item_of(3.1415) )
  call set( map, 'string', Item_of('char string') )
  call set( map, 'ref', Item_of(ref_of(val)) )
  call set( map, 'bool', Item_of(.true.) )

  map2 = map

  val => pop( map2, 'string' )
  print *, str(string(val))

  print *, unset( map2, 'absent' )
  print *, unset( map2, 'bool' )

  val => get( map, 'bla & text' )
  val = 7.34
  call set( map, 'key', Item_of('value') )

  print *, hasKey( map, 'key' )
  call remove( map, 'key' )
  print *, hasKey( map, 'key' )

  map = map2

  call initialize( map2, 10, 569 )
  do i = 0, 999
    write(key,"(A3,I6)") "key", i
    call set( map2, key, Item_of(i) )
  end do

  do i = 0, 999
    write(key,"(A3,I6)") "key", i
    if (i /= int32( get( map2, key ) )) &
      print *, "mismatch for value ", i
  end do

  call test_HashMap( 100000, 0, 997 )
  call test_HashMap( 100000, 5000, 5000 )
  call test_HashMap( 100000, 5000, 5000 )
  !call test_HashMap( 10000, 10 )      ! 0.29, 0.31, 0.062
  !call test_HashMap( 10000, 100 )     ! 0.31, 0.093, ~0
  !call test_HashMap( 10000, 1000 )    ! 1.17, 0.047, 1.5
  !call test_HashMap( 100000, 10 )     ! 38.5, 37.1, 1.13
  !call test_HashMap( 100000, 100 )    ! 5.85, 4.32, 1.03
  !call test_HashMap( 100000, 1000 )   ! 3.46, 0.75, 1.0
  !call test_HashMap( 1000000, 1000 )  ! 65.0, 62.95, 145.4

  call delete( map )
  call delete( map2 )
  call hashmap_clear_cache()
end

