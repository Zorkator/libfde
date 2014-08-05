
module base_types
  use iso_c_binding
  use generic_ref
  use type_info
  implicit none

  !_TypeReference_declare( public, bool,       logical,     scalar )
  !_TypeReference_declare( public, int8,       integer*1,   scalar )
  !_TypeReference_declare( public, int16,      integer*2,   scalar )
  !_TypeReference_declare( public, int32,      integer*4,   scalar )
  !_TypeReference_declare( public, int64,      integer*8,   scalar )
  !_TypeReference_declare( public, real32,     real*4,      scalar )
  !_TypeReference_declare( public, real64,     real*8,      scalar )
  !_TypeReference_declare( public, real128,    real*16,     scalar )
  !_TypeReference_declare( public, complex32,  complex*8,   scalar )
  !_TypeReference_declare( public, complex64,  complex*16,  scalar )
  !_TypeReference_declare( public, complex128, complex*32,  scalar )
  !_TypeReference_declare( public, c_ptr,      type(c_ptr), scalar )

  contains

  !_TypeReference_implementAll()

end module

