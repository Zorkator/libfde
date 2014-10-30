
#include "adt/itfUtil.fpp"

module adt_item__
  use adt_item
  use adt_typeinfo
  use adt_string
  use adt_ref
  use iso_c_binding

  type (String_t) :: string_var
  type (Ref_t)    :: ref_var

# define Item_t  Item_t__impl__

  interface
    function item_reshape( self, new_typeInfo ) result(res)
      import Item_t, TypeInfo_t
      type(Item_t)                         :: self
      type(TypeInfo_t), target, intent(in) :: new_typeInfo
      logical                              :: res
    end function
  end interface
end module

! implement constructor routines

# define _implementConstructor_(typeId, baseType, proto) \
    function _paste(item_of_,typeId)( val ) result(res) ;\
      use adt_item__; implicit none                     ;\
      baseType             :: val                       ;\
      baseType,    pointer :: ptr                       ;\
      type(Item_t), target :: res                       ;\
      if (item_reshape( res, static_type(val) )) then   ;\
        call res%typeInfo%initProc( res, 1, proto )     ;\
      end if                                            ;\
      call c_f_pointer( c_loc(res%data(1)), ptr )       ;\
      ptr = val                                         ;\
    end function

  _implementConstructor_(bool,       logical,        0)
  _implementConstructor_(int8,       integer*1,      0)
  _implementConstructor_(int16,      integer*2,      0)
  _implementConstructor_(int32,      integer*4,      0)
  _implementConstructor_(int64,      integer*8,      0)
  _implementConstructor_(real32,     real*4,         0)
  _implementConstructor_(real64,     real*8,         0)
  _implementConstructor_(complex32,  complex*8,      0)
  _implementConstructor_(complex64,  complex*16,     0)
  _implementConstructor_(c_void_ptr, type(c_ptr),    0)
  _implementConstructor_(string,     type(String_t), temporary_string)
  _implementConstructor_(ref,        type(Ref_t),    temporary_ref)


  function item_of_charString( val ) result(res)
    use adt_item__
    implicit none
    character(len=*)        :: val
    type(String_t), pointer :: ptr
    type(Item_t),    target :: res

    if (item_reshape( res, static_type(string_var) )) &
      call res%typeInfo%initProc( res%data, 1, temporary_string )
    call c_f_pointer( c_loc(res%data(1)), ptr )
    call assign( ptr, val )
  end function


  function item_of_refencoding( val ) result(res)
    use adt_item__
    implicit none
    type(RefEncoding_t), dimension(:) :: val
    type(Ref_t),              pointer :: ptr
    type(Item_t),              target :: res
    if (item_reshape( res, static_type(ref_var) )) &
      call res%typeInfo%initProc( res%data, 1, temporary_ref )
    call c_f_pointer( c_loc(res%data(1)), ptr )
    call assign( ptr, val )
  end function



  function item_reshape( self, new_typeInfo ) result(res)
    use adt_item__, only: Item_t, TypeInfo_t
    implicit none
    type(Item_t)                         :: self
    type(TypeInfo_t), target, intent(in) :: new_typeInfo
    logical                              :: res

    res = .false.
    if (.not. associated( self%typeInfo, new_typeInfo )) then
      ! check if old type needs to be deleted ...
      if (associated( self%typeInfo )) then
        if (associated( self%typeInfo%deleteProc )) &
          call self%typeInfo%deleteProc( self%data )
      end if
      ! return if new type needs to be initialized ...
      res = associated( new_typeInfo%initProc )
      self%typeInfo => new_typeInfo
    end if
  end function

