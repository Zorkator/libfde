
program testinger
  use hash_map
  use var_item
  use dynamic_string
  use generic_ref
  implicit none

  type(HashMap_t)                :: map, map2
  type(VarItem_t),       pointer :: val
  type(DynamicString_t), pointer :: s

  call initialize( map )
  call initialize( map, 10, 100 )
  call initialize( map, -10, 100 )
  call initialize( map, 0, 100 )
  call initialize( map, 0, -1 )
  call initialize( map, 2, 1 )
  call initialize( map2, 2, 1 )

  call set( map, 'bla & text', VarItem_of('value string') )

  val => get( map, 'testinger' )
  val = 42

  val => get( map, 'bla & text' )
  s => string(val)
  print *, str(s)

  call set( map, 'val-ref', VarItem_of(ref_of(val)) )

  val => get( map, 'val-ref' )
  s => DynamicString( ref(val) )
  print *, str(s)
  val = 'replace ref by text'

  val => setdefault( map, 'bool' )
  val => setdefault( map, 'float', VarItem_of(4.234) )
  val => setdefault( map, 'string', VarItem_of('char string') )
  val => setdefault( map, 'float', VarItem_of('char string') )

  call clear( map )

  call set( map, 'four', VarItem_of(4) )
  call set( map, 'float', VarItem_of(3.1415) )
  call set( map, 'string', VarItem_of('char string') )
  call set( map, 'ref', VarItem_of(ref_of(val)) )
  call set( map, 'bool', VarItem_of(.true.) )

  map2 = map

  val => pop( map2, 'string' )
  print *, str(string(val))

  print *, unset( map2, 'absent' )
  print *, unset( map2, 'bool' )

  val => get( map, 'bla & text' )
  val = 7.34
  call set( map, 'key', VarItem_of('value') )

  print *, hasKey( map, 'key' )
  call remove( map, 'key' )
  print *, hasKey( map, 'key' )

  map = map2

  call delete( map )
  call delete( map2 )
  call hm_clear_cache()
end

