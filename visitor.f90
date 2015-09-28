
#include "adt/itfUtil.fpp"

module adt_visitor
  use adt_typeinfo
  use adt_ostream
  implicit none
  private

  type, public :: Visitor_t
    procedure(), nopass, pointer :: visit => null()
  end type

  type, public :: StreamVisitor_t
    type(Visitor_t) :: super
    type(ostream_t) :: stream
  end type

  public :: Visitor, StreamVisitor

  interface Visitor       ; module procedure visitor_create       ; end interface
  interface StreamVisitor ; module procedure streamvisitor_create ; end interface

  contains

!_PROC_EXPORT(visitor_create)
  function visitor_create( func ) result(res)
    external        :: func
    type(Visitor_t) :: res
    res%visit => func    
  end function

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

