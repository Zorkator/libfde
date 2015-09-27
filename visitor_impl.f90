
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
    type(ostream_t)       :: tmp

    tmp = vstr%stream
    if (ti%typeId == "HashNode") &
      call newline( vstr%stream, 0 )

    call ti%streamProc( obj, vstr%stream )
    vstr%stream = tmp
  end subroutine
    
