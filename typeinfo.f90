
#include "adt/itfUtil.fpp"

module adt_typeinfo
  implicit none


  type, public :: TypeInfo_t
    character(32)             :: typeId;     integer*2 :: typeId_term   = 0
    character(32)             :: baseType;   integer*2 :: baseType_term = 0
    integer*4                 :: byteSize    =  0
    integer*4                 :: rank        =  0
    type(TypeInfo_t), pointer :: subtype     => null()
    logical                   :: initialized = .false.

    ! type specific subroutines called by generic interfaces 
    procedure(), nopass, pointer :: assignProc   => null()
    procedure(), nopass, pointer :: cloneObjProc => null()
    procedure(), nopass, pointer :: cloneRefProc => null()
    procedure(), nopass, pointer :: deleteProc   => null()
    procedure(), nopass, pointer :: initProc     => null()
    procedure(), nopass, pointer :: shapeProc    => null()
  end type

  
  type, public :: TypeInfo_ptr_t
    type(TypeInfo_t), pointer :: ptr
  end type


  type, public :: void_t
    integer, pointer :: ptr
  end type

  type(TypeInfo_t), target :: type_void = TypeInfo_t( "void", 0, "", 0, 0, 0, null(), .true., &
                                                       null(), null(), null(), null(), null(), null() )


  interface
    subroutine init_TypeInfo( self, typeId, baseType, bitSize, rank, subtype, &
                              assignProc, cloneObjProc, cloneRefProc, deleteProc, initProc, shapeProc )
      import TypeInfo_t
      type(TypeInfo_t),    intent(inout) :: self
      character(len=*),       intent(in) :: typeId, baseType
      integer*4,              intent(in) :: bitSize
      integer*4,              intent(in) :: rank
      type(TypeInfo_t), target, optional :: subtype
      procedure(),              optional :: assignProc, cloneObjProc, cloneRefProc, deleteProc, initProc, shapeProc
    end subroutine
  end interface

end module

