
#include "adt/itfUtil.fpp"

module adt_sort
  implicit none

  interface
    logical &
    function ValLower_itf( idxL, idxR )
      integer :: idxL, idxR
    end function

    subroutine ValSwap_itf( idxL, idxR )
      integer :: idxL, idxR
    end subroutine
  end interface

# define _swap(a, b) \
    if ((a) /= (b)) then; call swap( a, b ); end if

contains

!_PROC_EXPORT(heapsort)
  subroutine heapsort( is_lower, swap, n )
    procedure(ValLower_itf) :: is_lower
    procedure(ValSwap_itf)  :: swap
    integer                 :: n, i

    do i = n / 2 - 1, 0, -1
      call heapify_( n, i )
    end do

    do i = n - 1, 1, -1
      _swap( 0+1, i+1 )
      call heapify_( i, 0 )
    end do
  contains

    recursive &
    subroutine heapify_( n, i )
      integer :: n, i, largest, left, right

      largest = i
      left    = 2*i + 1
      right   = 2*i + 2

      if (left < n) then
        if (is_lower( largest+1, left+1 )) &
          largest = left
      end if

      if (right < n) then
        if (is_lower( largest+1, right+1 )) &
          largest = right
      end if

      if (largest /= i) then
        _swap( i+1, largest+1 )
        call heapify_( n, largest )
      end if
    end subroutine
  end subroutine


!_PROC_EXPORT(qsort)
  subroutine qsort( is_lower, swap, high, low )
    procedure(ValLower_itf) :: is_lower
    procedure(ValSwap_itf)  :: swap
    integer                 :: high, low_
    integer,       optional :: low

    _optArg( low_, low, 1 )
    call sort_range_( low_, high )
  contains

    recursive &
    subroutine sort_range_( low, high )
      integer :: low, high, pIdx

      if (low < high) then
        pIdx = partition_( low, high )
        call sort_range_( low, pIdx - 1 )
        call sort_range_( pIdx + 1, high )
      end if
    end subroutine

    integer &
    function partition_( low, high ) result(idx)
      integer :: low, high, j

      idx = low
      do j = low, high - 1
        if (is_lower( j, high )) then
          _swap( idx, j )
          idx = idx + 1
        end if
      end do
      _swap( idx, high )
    end function
  end subroutine
end module

