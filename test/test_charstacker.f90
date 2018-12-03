
module test_charstacker
  use fde_charstacker
  implicit none

  character(len=255)  :: path
  type(CharStacker_t) :: chrStack

  contains

  subroutine test_stack()
    chrStack = CharStacker( path, 3 )
    call walk_( 5 )
    call delete( chrStack )

  contains
  
    recursive &
    subroutine walk_( lvl )
      integer, value :: lvl

      print *, get( chrStack ), size( chrStack ), len( chrStack )
      if (lvl >= 0) then
        call push( chrStack, "_blub" )
          call walk_( lvl - 1 )
        call pop( chrStack )

        call push( chrStack, "_bla" )
          call walk_( lvl - 2 )
        call pop( chrStack )
      end if
    end subroutine
  end subroutine

end module

program charstacking
  use test_charstacker 

  call delete( chrStack )
  call initialize( chrStack, path )
  call test_stack()
end program

