
program testinger
  use hash_map
  implicit none

  type(HashMap_t) :: map

  call initialize( map )
  call initialize( map, 10, 100 )
  call initialize( map, -10, 100 )
  call initialize( map, 0, 100 )
  call initialize( map, 0, -1 )
  call initialize( map, 2, 1 )


  call delete( map )

end

