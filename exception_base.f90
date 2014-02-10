
#include "exception_base.fpp"

module exception_base
  use string_ref
  implicit none

  type NeverProvideThis
    ! empty
  end type

  interface tryCall
    _TryITF( try_sub_1, _0Args )
    _end_TryITF

    _TryITF( try_sub_2, _1Args )
      integer*4 :: arg1
    _end_TryITF

    _TryITF( try_sub_3, _1Args )
      type (StringRef) :: arg1
    _end_TryITF

    _TryITF( try_sub_4, _2Args )
      integer*4        :: arg1
      type (StringRef) :: arg2
    _end_TryITF

    _TryITF( try_sub_5, _3Args )
      type (StringRef) :: arg1, arg2
      integer*4        :: arg3
    _end_TryITF
  end interface

  interface
    subroutine throwException( code ) bind(C,name="throwException")
    use, intrinsic :: iso_c_binding
    integer(kind=c_int), value :: code
    end subroutine
  end interface

end module

