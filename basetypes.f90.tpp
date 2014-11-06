
!                . o O (needed for generated procedure exports)
#include "adt/itfUtil.fpp"

module adt_basetypes
  use iso_c_binding
  use adt_ref
  use adt_list
  use adt_string
  implicit none
  private

  !_TypeGen_declare_RefType( public, bool,       logical,     scalar )
  !_TypeGen_declare_RefType( public, int8,       integer*1,   scalar )
  !_TypeGen_declare_RefType( public, int16,      integer*2,   scalar )
  !_TypeGen_declare_RefType( public, int32,      integer*4,   scalar )
  !_TypeGen_declare_RefType( public, int64,      integer*8,   scalar )
  !_TypeGen_declare_RefType( public, real32,     real*4,      scalar )
  !_TypeGen_declare_RefType( public, real64,     real*8,      scalar )
  !_TypeGen_declare_RefType( public, real128,    real*16,     scalar )
  !_TypeGen_declare_RefType( public, complex32,  complex*8,   scalar )
  !_TypeGen_declare_RefType( public, complex64,  complex*16,  scalar )
  !_TypeGen_declare_RefType( public, complex128, complex*32,  scalar )
  !_TypeGen_declare_RefType( public, c_void_ptr, type(c_ptr), scalar )

  !_TypeGen_declare_ListNode( public, bool,       logical,     scalar )
  !_TypeGen_declare_ListNode( public, int8,       integer*1,   scalar )
  !_TypeGen_declare_ListNode( public, int16,      integer*2,   scalar )
  !_TypeGen_declare_ListNode( public, int32,      integer*4,   scalar )
  !_TypeGen_declare_ListNode( public, int64,      integer*8,   scalar )
  !_TypeGen_declare_ListNode( public, real32,     real*4,      scalar )
  !_TypeGen_declare_ListNode( public, real64,     real*8,      scalar )
  !_TypeGen_declare_ListNode( public, real128,    real*16,     scalar )
  !_TypeGen_declare_ListNode( public, complex32,  complex*8,   scalar )
  !_TypeGen_declare_ListNode( public, complex64,  complex*16,  scalar )
  !_TypeGen_declare_ListNode( public, complex128, complex*32,  scalar )
  !_TypeGen_declare_ListNode( public, c_void_ptr, type(c_ptr), scalar )
  
  !_TypeGen_declare_ListNode( public, ref, type(Ref_t),        scalar )
  !_TypeGen_declare_ListNode( alias, ref, type(RefEncoding_t), dimension(:) )

  !_TypeGen_declare_ListNode( public, String, type(String_t),  scalar )
  !_TypeGen_declare_ListNode( alias, String, character(len=*), scalar )
  !_TypeGen_declare_ListNode( alias, String, character(len=1), dimension(:) )

  contains

  !_TypeGen_implementAll()

end module

