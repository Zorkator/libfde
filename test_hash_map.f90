
program testinger
  use hash_map
  use var_item
  use dynamic_string
  implicit none

  type(HashMap_t)                :: map
  type(VarItem_t),       pointer :: val
  type(DynamicString_t), pointer :: s

  call initialize( map )
  call initialize( map, 10, 100 )
  call initialize( map, -10, 100 )
  call initialize( map, 0, 100 )
  call initialize( map, 0, -1 )
  call initialize( map, 2, 1 )

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

  call clear( map )

  val => get( map, 'bla & text' )
  val = 7.34
  call set( map, 'key', VarItem_of('value') )

  call delete( map )
  call hm_clear_cache()
end

