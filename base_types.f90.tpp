
module base_types
  use iso_c_binding
  use generic_ref
  use abstract_list
  use dynamic_string
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

  !_TypeGen_declare_ListItem( public, bool,       logical,     scalar )
  !_TypeGen_declare_ListItem( public, int8,       integer*1,   scalar )
  !_TypeGen_declare_ListItem( public, int16,      integer*2,   scalar )
  !_TypeGen_declare_ListItem( public, int32,      integer*4,   scalar )
  !_TypeGen_declare_ListItem( public, int64,      integer*8,   scalar )
  !_TypeGen_declare_ListItem( public, real32,     real*4,      scalar )
  !_TypeGen_declare_ListItem( public, real64,     real*8,      scalar )
  !_TypeGen_declare_ListItem( public, real128,    real*16,     scalar )
  !_TypeGen_declare_ListItem( public, complex32,  complex*8,   scalar )
  !_TypeGen_declare_ListItem( public, complex64,  complex*16,  scalar )
  !_TypeGen_declare_ListItem( public, complex128, complex*32,  scalar )
  !_TypeGen_declare_ListItem( public, c_void_ptr, type(c_ptr), scalar )
  
  !_TypeGen_declare_ListItem( public, ref, type(GenericRef_t), scalar )
  !_TypeGen_declare_ListItem( alias, ref, type(GenericRef_Encoding_t), dimension(:) )

  !_TypeGen_declare_ListItem( public, DynamicString, type(DynamicString_t), scalar )
  !_TypeGen_declare_ListItem( alias, DynamicString, character(len=*), scalar )

  contains

  !_TypeGen_implementAll()

end module

