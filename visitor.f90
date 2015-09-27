
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

  contains

  function Visitor( func ) result(res)
    external        :: func
    type(Visitor_t) :: res
    res%visit => func    
  end function

  function StreamVisitor( channel ) result(res)
    integer               :: channel
    type(StreamVisitor_t) :: res

    interface
      subroutine stream_visit_( vstr, obj, ti )
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

