
#include "fde/itfUtil.fpp"

module fde_visitor
  use fde_typeinfo
  implicit none
  private

  type, public :: Visitor_t
    integer*4                    :: level  =  0
    procedure(), nopass, pointer :: visit  => null()
    procedure(), nopass, pointer :: enter_ => null()
    procedure(), nopass, pointer :: leave_ => null()
    procedure(), nopass, pointer :: group_ => null()
  end type

  public :: Visitor, is_valid, enter, leave, group

  interface Visitor  ; module procedure visitor_create   ; end interface
  interface is_valid ; module procedure visitor_is_valid ; end interface
  interface enter    ; module procedure visitor_enter    ; end interface
  interface leave    ; module procedure visitor_leave    ; end interface
  interface group    ; module procedure visitor_group    ; end interface

  contains

!_PROC_EXPORT(visitor_create)
  function visitor_create( func, enter, leave, group ) result(res)
    procedure()           :: func
    procedure(), optional :: enter, leave, group
    type(Visitor_t) :: res
    res%visit  => func
    res%enter_ => enter
    res%leave_ => leave
    res%group_ => group
  end function


!_PROC_EXPORT(visitor_is_valid)
  pure &
  function visitor_is_valid( self ) result(res)
    type(Visitor_t), intent(in) :: self
    logical                     :: res
    res = self%level >= 0
  end function


!_PROC_EXPORT(visitor_enter)
  subroutine visitor_enter( self )
    type(Visitor_t) :: self
    self%level = self%level + 1
    if (associated(self%enter_)) &
      call self%enter_( self )
  end subroutine


!_PROC_EXPORT(visitor_leave)
  subroutine visitor_leave( self )
    type(Visitor_t) :: self
    self%level = self%level - 1
    if (associated(self%leave_)) &
      call self%leave_( self )
  end subroutine


!_PROC_EXPORT(visitor_group)
  subroutine visitor_group( self, num )
    type(Visitor_t) :: self
    integer*4       :: num
    if (associated(self%group_)) &
      call self%group_( self, num )
  end subroutine

end module

