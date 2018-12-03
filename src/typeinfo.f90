
#include "fde/itfUtil.fpp"

module fde_typeinfo
  use fde_memoryref
  implicit none
  public

  type, public, bind(c) :: TypeSpecs_t
    type(MemoryRef_t)      :: typeId, baseType
    integer(kind=c_size_t) :: byteSize  = 0
    integer(kind=c_size_t) :: rank      = 0
    integer(kind=c_size_t) :: streamLen = 0
    type(c_ptr)            :: subtype   = C_NULL_PTR
  end type


  type, public :: TypeInfo_t
    type(TypeSpecs_t)         :: typeSpecs
    character(32)             :: typeId;     integer*2 :: typeId_term   = 0
    character(32)             :: baseType;   integer*2 :: baseType_term = 0
    type(TypeInfo_t), pointer :: subtype     => null()
    logical                   :: initialized = .false.

    ! type specific subroutines called by generic interfaces 
    procedure(), nopass, pointer :: acceptProc   => null()
    procedure(), nopass, pointer :: assignProc   => null()
    procedure(), nopass, pointer :: cloneObjProc => null()
    procedure(), nopass, pointer :: cloneRefProc => null()
    procedure(), nopass, pointer :: deleteProc   => null()
    procedure(), nopass, pointer :: initProc     => null()
    procedure(), nopass, pointer :: shapeProc    => null()
    procedure(), nopass, pointer :: streamProc   => null()
  end type

  
  type, public :: TypeInfo_ptr_t
    type(TypeInfo_t), pointer :: ptr => null()
  end type


  type, public :: void_t
    type(TypeInfo_t), pointer :: ptr
  end type


  interface
    subroutine typeinfo_init( self, typeId, baseType, bitSize, rank, subtype, &
                              acceptProc, assignProc, cloneObjProc, cloneRefProc, deleteProc, initProc, shapeProc, streamProc, tryStreamProc )
      import TypeInfo_t
      type(TypeInfo_t),    intent(inout) :: self
      character(len=*),       intent(in) :: typeId, baseType
      integer*4,              intent(in) :: bitSize
      integer*4,              intent(in) :: rank
      type(TypeInfo_t), target, optional :: subtype
      procedure(),              optional :: acceptProc, assignProc, cloneObjProc, cloneRefProc, deleteProc, initProc, shapeProc &
                                          , streamProc, tryStreamProc
    end subroutine
  end interface

  interface void_type
    function typeinfo_void_type() result(res)
      import TypeInfo_t
      type(TypeInfo_t), pointer :: res
    end function
  end interface

end module

