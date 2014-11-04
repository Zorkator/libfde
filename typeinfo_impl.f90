
#include "adt/itfUtil.fpp"


!_PROC_EXPORT(typeinfo_object_size)
  integer(kind=4) &
  function typeinfo_object_size() result(res)
    use adt_typeinfo; implicit none
    type (TypeInfo_t) :: tmp
    res = storage_size(tmp) / 8
  end function


  !**
  ! init_typeinfo initializes TypeInfo structure.
  ! @param self         - the TypeInfo to initialize
  ! @param typeId       - the type's id string (e.g. double)
  ! @param baseType     - the type's base string (e.g. real*8)
  ! @param bitSize      - the storage size of the type in bytes (=> storage_size(type))
  ! @param rank         - the rank of the type
  ! @param assignProc   - the subroutine to assign a variable      : subroutine assign( lhs, rhs )
  ! @param cloneObjProc - the function to clone an object reference: subroutine clone( tgt, src )
  ! @param cloneRefProc - the function to clone an object          : subroutine clone( tgt, src )
  ! @param deleteProc   - the subroutine to delete a variable      : subroutine delete( var )
  ! @param initProc     - the subroutine to initialize a variable  : subroutine init( var, hardness )
  ! @param shapeProc    - the function to inspect the shape        : subroutine shape( var, res, rank )
  !*
!PROC_EXPORT_1REF(init_typeinfo, self)
  subroutine init_typeinfo( self, typeId, baseType, bitSize, rank, subtype, &
                            assignProc, cloneObjProc, cloneRefProc, deleteProc, initProc, shapeProc )
    use adt_typeinfo, only: TypeInfo_t
    type(TypeInfo_t),    intent(inout) :: self
    character(len=*),       intent(in) :: typeId, baseType
    integer*4,              intent(in) :: bitSize
    integer*4,              intent(in) :: rank
    type(TypeInfo_t), target, optional :: subtype
    procedure(),              optional :: assignProc, cloneObjProc, cloneRefProc, deleteProc, initProc, shapeProc

    self%typeId   =  adjustl(typeId);   self%typeId_term   = 0
    self%baseType =  adjustl(baseType); self%baseType_term = 0
    self%byteSize =  bitSize/8
    self%rank     =  rank
    self%subtype  => subtype

    ! pre-initialize optional arguments
    self%assignProc   => null()
    self%cloneObjProc => null()
    self%cloneRefProc => null()
    self%deleteProc   => null()
    self%initProc     => null()
    self%shapeProc    => null()

    if (present(assignProc))   self%assignProc   => assignProc
    if (present(cloneObjProc)) self%cloneObjProc => cloneObjProc
    if (present(cloneRefProc)) self%cloneRefProc => cloneRefProc
    if (present(deleteProc))   self%deleteProc   => deleteProc
    if (present(initProc))     self%initProc     => initProc
    if (present(shapeProc))    self%shapeProc    => shapeProc
    self%initialized = .true.
  end subroutine

