
#include "adt/itfUtil.fpp"

module impl_visitor__
  use adt_visitor
  use adt_typeinfo
  use adt_hashmap
  use adt_basetypes
  use adt_ostream
  implicit none
  
end module

  subroutine stream_visit_( vstr, obj, ti )
    use impl_visitor__
    implicit none
    type(StreamVisitor_t) :: vstr
    type(void_t)          :: obj
    type(TypeInfo_t)      :: ti
    logical               :: no_break

    no_break = (ti%typeId == "HashNode")

    if (no_break) &
      call newline( vstr%stream, 0 )

    call indent( vstr%stream, vstr%super%level )
    call ti%streamProc( obj, ti, vstr%stream )

    if (no_break) &
      call newline( vstr%stream, 1 )
  end subroutine
    
