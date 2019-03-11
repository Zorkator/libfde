
#include "fde/itfUtil.fpp"

module fde_streamvisitor
  use fde_visitor
  use fde_ostream
  use fde_typeinfo
  implicit none
  private

  type, public :: StreamVisitor_t
    type(Visitor_t) :: super
    type(ostream_t) :: stream
  end type

  public :: StreamVisitor
  public :: is_valid, enter, leave, group

  interface StreamVisitor ; module procedure streamvisitor_create ; end interface

  contains

!_PROC_EXPORT(streamvisitor_create)
  function streamvisitor_create( channel ) result(res)
    integer               :: channel
    type(StreamVisitor_t) :: res

    res%super  = Visitor( streamvisitor_visit_ &
                        , enter = streamvisitor_enter_ &
                        , leave = streamvisitor_leave_ &
                        , group = streamvisitor_group_ )
    res%stream = ostream( channel )
  end function


  subroutine streamvisitor_visit_( vstr, obj, ti )
    type(StreamVisitor_t) :: vstr
    type(void_t)          :: obj
    type(TypeInfo_t)      :: ti
    call ti%streamProc( obj, ti, vstr%stream )
  end subroutine
 
  subroutine streamvisitor_enter_( self )
    type(StreamVisitor_t) :: self
    call indent( self%stream, self%super%level )
    call fuselines( self%stream, 0 )
  end subroutine

  subroutine streamvisitor_leave_( self )
    type(StreamVisitor_t) :: self
    call indent( self%stream, self%super%level )
    call fuselines( self%stream, 0 )
  end subroutine

  subroutine streamvisitor_group_( self, num )
    type(StreamVisitor_t) :: self
    integer*4             :: num
    if (num > 0) &
      call fuselines( self%stream, num - 1 )
  end subroutine

end module

