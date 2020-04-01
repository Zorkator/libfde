
#include "fde/string.fpp"

module test_sorting
  use fde_sort
  use fde_exception
  contains

  subroutine test_sort()
    real(8), dimension(:), allocatable :: array
    integer                            :: i, cmp, swp, timing

    allocate( array(10000) )
    cmp = 0; swp = 0

#   define _sortArrayBy( sort, msg )             \
      timing = startTimer()                     ;\
      call sort( is_lower_, swap_, size(array) );\
      call getTiming( _str(sort)//msg, timing ) ;\
      call chk_array()

    do i = 1, size(array)
      array(i) = size(array)-i
    end do
    _sortArrayBy( qsort, " reversed" )
    _sortArrayBy( qsort, " sorted" )

    do i = 1, size(array)
      array(i) = size(array)-i
    end do
    _sortArrayBy( heapsort, " reversed" )
    _sortArrayBy( heapsort, " sorted" )

    call init_random_seed(0)
    call random_number( array )
    _sortArrayBy( qsort, " random" )
    call init_random_seed(0)
    call random_number( array )
    _sortArrayBy( heapsort, " random" )

  contains

    logical &
    function is_lower_( l, r ) result(res)
      integer :: l, r
      res = array(l) < array(r)
      cmp = cmp + 1
    end function

    logical &
    function is_greater_( l, r ) result(res)
      integer :: l, r
      res = array(l) > array(r)
      cmp = cmp + 1
    end function

    subroutine swap_( l, r )
      integer :: l, r
      real(8) :: t
      t = array(l)
      array(l) = array(r)
      array(r) = t
      swp = swp + 1
    end subroutine

    subroutine chk_array()
      do i = 2, size(array)
        if (array(i-1) > array(i)) &
          call throw( AssertionError, "unsorted sequence!" )
      end do
      print *, cmp, "comparisons", swp, "swaps"
      cmp = 0; swp = 0
    end subroutine
  end subroutine


  subroutine init_random_seed( val )
    integer              :: val
    integer, allocatable :: seed(:)
    integer              :: n
    call random_seed( size=n )
    allocate( seed(n) )
    seed = val
    call random_seed( put=seed )
  end subroutine

  function startTimer() result(res)
    integer :: res
    call system_clock( res )
  end function

  subroutine getTiming( descr, time_begin )
    character(len=*) :: descr
    integer          :: time_begin, time_end, rate
    call system_clock( time_end )
    call system_clock( count_rate=rate )
    print*, descr, ': ', (time_end - time_begin)/real(rate), ' seconds'
  end subroutine

end module

program testing
  use test_sorting 

  call test_sort()
end program

