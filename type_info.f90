
#include "adt/itfUtil.fpp"

module type_info
  implicit none


  type, public :: TypeInfo_t
    character(32)             :: typeId;     integer*2, private :: typeId_term   = 0
    character(32)             :: baseType;   integer*2, private :: baseType_term = 0
    integer*4                 :: byteSize    =  0
    integer*4                 :: rank        =  0
    type(TypeInfo_t), pointer :: subtype     => null()
    logical                   :: initialized = .false.

    ! type specific subroutines called by generic interfaces 
    procedure(), nopass, pointer :: assignProc   => null()
    procedure(), nopass, pointer :: castProc     => null()
    procedure(), nopass, pointer :: cloneObjProc => null()
    procedure(), nopass, pointer :: cloneRefProc => null()
    procedure(), nopass, pointer :: deleteProc   => null()
    procedure(), nopass, pointer :: initProc     => null()
    procedure(), nopass, pointer :: shapeProc    => null()
  end type


  type, public :: void_t
    integer, pointer :: ptr
  end type

  type(TypeInfo_t), target :: type_void = TypeInfo_t( "void", 0, "", 0, 0, 0, null(), .true., &
                                                       null(), null(), null(), null(), null(), null(), null() )

  contains

  !**
  ! init_TypeInfo initializes TypeInfo structure.
  ! @param self         - the TypeInfo to initialize
  ! @param typeId       - the type's id string (e.g. double)
  ! @param baseType     - the type's base string (e.g. real*8)
  ! @param bitSize      - the storage size of the type in bytes (=> storage_size(type))
  ! @param rank         - the rank of the type
  ! @param assignProc   - the subroutine to assign a variable      : subroutine assign( lhs, rhs )
  ! @param castProc     - the subroutine to cast a cptr to fortran : subroutine cast( ptr_c, ptr_f )
  ! @param cloneObjProc - the function to clone an object reference: subroutine clone( tgt, src )
  ! @param cloneRefProc - the function to clone an object          : subroutine clone( tgt, src )
  ! @param deleteProc   - the subroutine to delete a variable      : subroutine delete( var )
  ! @param initProc     - the subroutine to initialize a variable  : subroutine init( var, hardness )
  ! @param shapeProc    - the function to inspect the shape        : subroutine shape( var, res, rank )
  !*
  !PROC_EXPORT_1REF( init_TypeInfo, self )
  subroutine init_TypeInfo( self, typeId, baseType, bitSize, rank, subtype, &
                            assignProc, castProc, cloneObjProc, cloneRefProc, deleteProc, initProc, shapeProc )
    type(TypeInfo_t),    intent(inout) :: self
    character(len=*),       intent(in) :: typeId, baseType
    integer*4,              intent(in) :: bitSize
    integer*4,              intent(in) :: rank
    type(TypeInfo_t), target, optional :: subtype
    procedure(),              optional :: assignProc, castProc, cloneObjProc, cloneRefProc, deleteProc, initProc, shapeProc

    self%typeId   =  adjustl(typeId);   self%typeId_term   = 0
    self%baseType =  adjustl(baseType); self%baseType_term = 0
    self%byteSize =  bitSize/8
    self%rank     =  rank
    self%subtype  => subtype

    ! pre-initialize optional arguments
    self%assignProc   => null()
    self%castProc     => null()
    self%cloneObjProc => null()
    self%cloneRefProc => null()
    self%deleteProc   => null()
    self%initProc     => null()
    self%shapeProc    => null()

    if (present(assignProc))   self%assignProc   => assignProc
    if (present(castProc))     self%castProc     => castProc
    if (present(cloneObjProc)) self%cloneObjProc => cloneObjProc
    if (present(cloneRefProc)) self%cloneRefProc => cloneRefProc
    if (present(deleteProc))   self%deleteProc   => deleteProc
    if (present(initProc))     self%initProc     => initProc
    if (present(shapeProc))    self%shapeProc    => shapeProc
    self%initialized = .true.
  end subroutine

end module type_info

