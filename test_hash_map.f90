
program testinger
  use adt_hashmap
  use adt_item
  use adt_string
  use adt_ref
  implicit none

  type(HashMap_t)         :: map, map2
  type(Item_t),   pointer :: val
  type(String_t), pointer :: s

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

  call delete( map )
  call delete( map2 )
  call hashmap_clear_cache()
end

