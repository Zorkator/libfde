
#include "fde/exception.fpp"

module test_dyncast
  use fde_item
  use fde_ref
  use fde_hashmap
  use fde_basetypes
  use fde_exception
  implicit none

    type(Ref_t),       target :: ref_
    type(Item_t),      target :: item_
    type(Ref_t),      pointer :: ref_ptr
    type(Item_t),     pointer :: item_ptr
    type(TypeInfo_t), pointer :: ti

  contains
  
  subroutine test_content_type()
    ref_ptr  => null()
    item_ptr => null()
    ti => content_type(ref_)     ; _assert(ti%typeid == 'void')
    ti => content_type(ref_ptr)  ; _assert(ti%typeid == 'void')
    ti => content_type(item_)    ; _assert(ti%typeid == 'void')
    ti => content_type(item_ptr) ; _assert(ti%typeid == 'void')

    item_ptr => item_
    ti => content_type(item_ptr) ; _assert(ti%typeid == 'void')
    item_ = 5
    ti => content_type(item_ptr) ; _assert(ti%typeid == 'int4')

    print *, "test_content_type: ok"
  end subroutine


  subroutine test_dynamic_cast()
    real*8             :: rval = 1.23
    real*8,    pointer :: rptr
    integer*4          :: val  = 42
    integer*4, pointer :: ptr

    item_ = 42        ; _assert( dynamic_cast( ptr, item_ ) )
    item_ = 4.2       ; _assert( .not. dynamic_cast( ptr, item_ )    .and. .not. associated(ptr) )
    item_ptr => null(); _assert( .not. dynamic_cast( ptr, item_ptr ) .and. .not. associated(ptr) )

    ref_ptr  => null() ; _assert( .not. dynamic_cast( ptr, ref_ptr ) .and. .not. associated(ptr) )
    ref_ = ref_of(val) ; _assert( dynamic_cast( ptr, ref_ )          .and.       ptr == 42 )
    ref_ = ref_of(rval); _assert( .not. dynamic_cast( ptr, ref_ )    .and. .not. associated(ptr) )
    ref_ptr  => ref_   ; _assert( .not. dynamic_cast( ptr, ref_ptr ) .and. .not. associated(ptr) )
    continue           ; _assert( dynamic_cast( rptr, ref_ptr )      .and.       rptr == 1.23 )
    item_ = ref_       ; _assert( dynamic_cast( rptr, item_ )        .and.       rptr == rval )

    print *, "test_dynamic_cast: ok"
  end subroutine

end module

program dyn_cast
  use test_dyncast 

  call test_content_type()
  call test_dynamic_cast()
end program

