
#include "fde/itfUtil.fpp"

module fde_sort
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
      _swap( 1, i+1 )
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
    integer,          value :: high
    integer,       optional :: low
    integer                 :: low_

    integer, dimension(:), allocatable :: stack
    integer                            :: tos, pIdx

    _optArg( low_, low, 1 )
    allocate( stack( high - low_ + 1 ) ); tos = 0

    call push_( low_, high )
    do while (tos > 0)
      call pop_( low_, high )
      pIdx = partition_( low_, high )
      call push_( low_, pIdx - 1 )
      call push_( pIdx + 1, high )
    end do
  contains

    subroutine push_( l, h )
      integer :: l, h
      if (l < h) then
        tos = tos + 2
        stack(tos-1:tos) = [l, h]
      end if
    end subroutine

    subroutine pop_( l, h )
      integer :: l, h
      l = stack(tos-1)
      h = stack(tos)
      tos = tos - 2
    end subroutine


    integer &
    function partition_( low, high ) result(idx)
      integer :: low, high, jdx

      idx = low
      do jdx = low, high - 1
        if (.not. is_lower( high, jdx )) then
          _swap( idx, jdx )
          idx = idx + 1
        end if
      end do
      if (idx == high) then; high = high - 1
                       else; call swap( idx, high )
      end if
    end function
  end subroutine
end module

