
#include "adt/itfUtil.fpp"

module type_info
  implicit none


  type, public :: TypeInfo
    character(32)                :: typeId;     integer*2, private :: typeId_term   = 0
    character(32)                :: baseType;   integer*2, private :: baseType_term = 0
    integer*4                    :: byteSize    =  0
    integer*4                    :: rank        =  0
    logical                      :: initialized = .false.

    ! type specific subroutines called by generic interfaces 
    procedure(), nopass, pointer :: assignProc  => null()
    procedure(), nopass, pointer :: shapeProc   => null()
    procedure(), nopass, pointer :: cloneProc   => null()
    procedure(), nopass, pointer :: deleteProc  => null()
  end type


  type(TypeInfo), target :: type_void = TypeInfo( "void", 0, "", 0, 0, 0, .true., &
                                                   null(), null(), null(), null() )

  contains

  !**
  ! init_TypeInfo initializes TypeInfo structure.
  ! @param self        - the TypeInfo to initialize
  ! @param typeId      - the type's id string (e.g. double)
  ! @param baseType    - the type's base string (e.g. real*8)
  ! @param bitSize     - the storage size of the type in bytes (=> storage_size(type))
  ! @param rank        - the rank of the type
  ! @param assignProc  - the subroutine to assign a variable: subroutine assign( lhs, rhs )
  ! @param deleteProc  - the subroutine to delete a variable: subroutine delete( var )
  ! @param shapeProc   - the function to inspect the shape  : function getShape( var ) return(res)
  ! @param cloneProc   - the function to clone a variable   : function getClone( var ) return(res)
  !*
  !PROC_EXPORT_1REF( init_TypeInfo, self )
  subroutine init_TypeInfo( self, typeId, baseType, bitSize, rank, &
                            assignProc, deleteProc, shapeProc, cloneProc )
    type(TypeInfo),    intent(inout) :: self
    character(len=*),     intent(in) :: typeId, baseType
    integer*4,            intent(in) :: bitSize
    integer*4,            intent(in) :: rank
    procedure(),            optional :: assignProc, deleteProc, shapeProc, cloneProc

    self%typeId   = adjustl(typeId);   self%typeId_term   = 0
    self%baseType = adjustl(baseType); self%baseType_term = 0
    self%byteSize = bitSize/8
    self%rank     = rank

    ! pre-initialize optional arguments
    self%assignProc => null()
    self%deleteProc => null()
    self%shapeProc  => null()
    self%cloneProc  => null()

    if (present(assignProc)) self%assignProc => assignProc
    if (present(deleteProc)) self%deleteProc => deleteProc
    if (present(shapeProc))  self%shapeProc  => shapeProc
    if (present(cloneProc))  self%cloneProc  => cloneProc
    self%initialized = .true.
  end subroutine

end module type_info

