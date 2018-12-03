
#include "fde/itfUtil.fpp"

module fde_bisect
  implicit none

  interface
    logical function ValLowerSeq_itf( idx ); import
      integer(8) :: idx
    end function
  end interface

contains

!_PROC_EXPORT(bisect_right)
  function bisect_right( val_lower_seq, lo, hi ) result(lo_)
    procedure(ValLowerSeq_itf) :: val_lower_seq
    integer(8),     intent(in) :: lo,  hi
    integer(8)                 :: lo_, hi_, mid

    lo_ = lo
    hi_ = hi
    do while (lo_ < hi_)
      mid = (lo_ + hi_)/2
      if (val_lower_seq(mid)) then; hi_ = mid
                              else; lo_ = mid + 1
      end if
    end do
  end function

end module

