
module mod_uservisitor
  use fde_visitor
  implicit none
  private

  public :: test_uservisitor

  type node_t
    type(node_t), pointer :: next => null()
    integer               :: value = -1
  end type

  type(node_t), target  :: list

  type CountVisitor_t
    type(Visitor_t)              :: super
    procedure(), nopass, pointer :: visit
    integer                      :: sum = 0
  end type

contains

  subroutine CountVisitor_step( v, n )
    type(CountVisitor_t) :: v
    type(node_t)         :: n
    v%sum = v%sum + 1
    call v%visit( v, n )
  end subroutine

  function CountVisitor( func ) result(res)
    external             :: func
    type(CountVisitor_t) :: res
    res%super%visit => CountVisitor_step
    res%visit       => func
  end function


  subroutine init_list()
    type(node_t), pointer :: ptr
    integer               :: i

    ptr => list
    do i = 1, 5
      allocate(ptr%next)
      ptr%value = i
      ptr => ptr%next
    end do
  end subroutine

  recursive &
  subroutine accept( n, v )
    type(node_t)    :: n
    type(visitor_t) :: v

    call v%visit( v, n )
    if (associated(n%next)) &
      call accept( n%next, v )
  end subroutine

  subroutine blub_( v, n )
    type(visitor_t) :: v
    type(node_t)    :: n
    print *, 'blub_', n%value
  end subroutine

  subroutine test_uservisitor()
    type(CountVisitor_t) :: v

    call init_list()
    call accept( list, Visitor(blub_) )
    v = CountVisitor(blub_)
    call accept( list, v%super )
    print *, v%sum
  end subroutine

end module

program testing
  use mod_uservisitor

  call test_uservisitor()
end program

