
#include "adt/itfUtil.fpp"

module adt_visitor
  use adt_typeinfo
  use adt_ostream
  implicit none
  private

  type, public :: Visitor_t
    procedure(), nopass, pointer :: visit  => null()
    integer*4                    :: level  =  0
    procedure(), nopass, pointer :: enter_ => null()
    procedure(), nopass, pointer :: leave_ => null()
  end type

  public :: Visitor, is_valid, enter, leave

  interface Visitor  ; module procedure visitor_create   ; end interface
  interface is_valid ; module procedure visitor_is_valid ; end interface
  interface enter    ; module procedure visitor_enter    ; end interface
  interface leave    ; module procedure visitor_leave    ; end interface


  type, public :: StreamVisitor_t
    type(Visitor_t) :: super
    type(ostream_t) :: stream
  end type

  public :: StreamVisitor

  interface StreamVisitor ; module procedure streamvisitor_create ; end interface

  contains

!_PROC_EXPORT(visitor_create)
  function visitor_create( func ) result(res)
    procedure()     :: func
    type(Visitor_t) :: res
    res%visit => func
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



!_PROC_EXPORT(streamvisitor_create)
  function streamvisitor_create( channel ) result(res)
    integer               :: channel
    type(StreamVisitor_t) :: res

    interface
      subroutine stream_visit_( vstr, ti, obj )
        import StreamVisitor_t, void_t, TypeInfo_t
        type(StreamVisitor_t) :: vstr
        type(void_t)          :: obj
        type(TypeInfo_t)      :: ti
      end subroutine
    end interface

    res%super  = Visitor( stream_visit_ )
    res%stream = ostream( channel )
  end function

end module

