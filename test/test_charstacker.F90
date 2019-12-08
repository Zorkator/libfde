
module test_charstacker
  use fde_charstacker
  implicit none

  character(len=255)  :: path
  type(CharStacker_t) :: chrStack

  contains

  subroutine test_stack()
    chrStack = CharStacker( path )
    call walk_( 5 )

  contains
  
    recursive &
    subroutine walk_( lvl )
      integer, value :: lvl

      call push( chrStack, "_blub" )
        print *, get( chrStack )

        if (lvl > 0) then
          call walk_( lvl - 1 )
        end if
      call pop( chrStack )
    end subroutine
  end subroutine

end module

program charstacking
  use test_charstacker 

  call test_stack()
end program

