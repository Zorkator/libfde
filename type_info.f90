
#include "adt/itfUtil.fpp"

module type_info
  implicit none

  type, public :: TypeInfo
    character(32)                :: typeId;     integer*2 :: typeId_term   = 0
    character(32)                :: baseType;   integer*2 :: baseType_term = 0
    integer                      :: byteSize    =  0
    integer                      :: rank        =  0
    procedure(), nopass, pointer :: assignFunc  => null() !< mandatory: assignment of datatype
    procedure(), nopass, pointer :: deleteFunc  => null() !< mandatory: deletion of datatype
    ! --- optional type specific functions ---
    procedure(), nopass, pointer :: usrFunc1    => null() !< shape inspector
    procedure(), nopass, pointer :: usrFunc2    => null() !< clone func
    procedure(), nopass, pointer :: usrFunc3    => null()
    logical                      :: initialized = .false.
  end type

  !interface operator (.string.); module procedure TypeInfo_get_string; end interface

  !interface assignFunc ; module procedure ti_assignFunc ; end interface
  !interface cloneFunc  ; module procedure ti_cloneFunc  ; end interface
  !interface inspectFunc; module procedure ti_inspectFunc; end interface
  !interface deleteFunc ; module procedure ti_deleteFunc ; end interface

  type (TypeInfo), target :: TypeInfo_void = TypeInfo("void", 0, "", 0, 0, 0, null(), null(), &
                                                      null(), null(), null(), .true.)

  contains

  !**
  ! TypeInfo_init initializes TypeInfo structure.
  ! @param self        - the TypeInfo to initialize
  ! @param typeId      - the type's id string (e.g. double)
  ! @param baseType    - the type's base string (e.g. real*8)
  ! @param byteSize    - the storage size of the type in bytes (=> storage_size(type)/8)
  ! @param rank        - the rank of the type (default: 0, so assume scalar)
  ! @param assignFunc  - the procedure to assign a variable of this type to another
  ! @param deleteFunc  - the procedure to delete a variable
  ! @param shapeFunc   - the procedure to inspect the shape
  ! @param cloneFunc   - optional type procedure 2
  !*
  !PROC_EXPORT_1REF( TypeInfo_init, self )
  subroutine TypeInfo_init( self, typeId, baseType, byteSize, rank, assignFunc, deleteFunc, usrFunc1, usrFunc2, usrFunc3 )
    type (TypeInfo),    intent(inout) :: self
    character(len=*),      intent(in) :: typeId, baseType
    integer,               intent(in) :: byteSize
    integer,     optional, intent(in) :: rank
    procedure(), optional             :: assignFunc, deleteFunc, usrFunc1, usrFunc2, usrFunc3

    self%typeId   = typeId;   self%typeId_term   = 0
    self%baseType = baseType; self%baseType_term = 0
    self%byteSize = byteSize

    ! pre-initialize optional arguments
    self%rank       =  0
    self%assignFunc => null()
    self%deleteFunc => null()
    self%usrFunc1   => null()
    self%usrFunc2   => null()
    self%usrFunc3   => null()

    if (present(rank))       self%rank       =  rank
    if (present(assignFunc)) self%assignFunc => assignFunc
    if (present(deleteFunc)) self%deleteFunc => deleteFunc
    if (present(usrFunc1))   self%usrFunc1   => usrFunc1
    if (present(usrFunc2))   self%usrFunc2   => usrFunc2
    if (present(usrFunc3))   self%usrFunc3   => usrFunc3
    self%initialized = .true.
  end subroutine


  !!PROC_EXPORT_1REF( TypeInfo_get_string, self )
  !function TypeInfo_get_string( self ) result(res)
  !  type (TypeInfo), intent(in) :: self
  !  character(len=60)           :: res, buff
  !  if (self%rank > 0) &
  !    write (buff, "(A,I02)"), " Array-rank: ", self%rank
  !  res  = trim(self%baseType) // buff
  !end function

end module type_info

!!# define _scalar()
!!# define _rank_0()                       _scalar()
!!# define _rank_1()                       , dimension(:)
!!# define _rank_2()                       , dimension(:,:)
!!# define _rank_3()                       , dimension(:,:,:)
!!# define _rank_4()                       , dimension(:,:,:,:)
!!# define _rank_5()                       , dimension(:,:,:,:,:)
!!# define _rank_6()                       , dimension(:,:,:,:,:,:)
!!# define _rank_7()                       , dimension(:,:,:,:,:,:,:)
!!
!!
!!# define _GENERIC_REF_func_inspect       usrFunc1 
!!# define _GENERIC_REF_func_clone         usrFunc2 
!!# define _GENERIC_REF_func_delete        usrFunc3 
!!# define _GENERIC_REF_func_none          noFunc
!!
!!# define _default( what, refId )         _paste4(GenericRef_,what,_,refId)
!!# define _none( what, refId )            _GENERIC_REF_func_none
!!# define _procedure( what )              what _eatArgs_2
!!
!!# define _GENERIC_REF_encode( refId )    _paste(GenericRef_encode_,refId)
!!# define _GENERIC_REF_decode( refId )    _paste(GenericRef_decode_,refId)
!!# define _GENERIC_REF_typeInfo( refId )  _paste(GenericRef_typeInfo_,refId)
!!
!!
!!# define _GENERIC_REF_TypeReference( showType, refId, baseType, dimType )                     \
!!  type, showType :: refId; baseType dimType(), pointer :: ptr; end type                      ;\
!!  type (TypeInfo), target :: _GENERIC_REF_typeInfo(refId)                                    ;\
!!  interface operator(.ref.);   module procedure _GENERIC_REF_encode(refId);   end interface  ;\
!!  interface operator(.refId.); module procedure _GENERIC_REF_decode(refId);   end interface
!!
!!# define PrivateTypeReference( refId, baseType, dimType ) \
!!  _GENERIC_REF_TypeReference(private,refId,baseType,dimType)
!!
!!# define PublicTypeReference( refId, baseType, dimType ) \
!!  _GENERIC_REF_TypeReference(public,refId,baseType,dimType)
!!
!!# define TypeReference  PrivateTypeReference
!!
!!
!!
!!# define _GENERIC_REF_ProcReference( showType, refId, baseType )                  \
!!  type, showType :: refId; procedure(baseType), nopass, pointer :: ptr; end type ;\
!!  type (TypeInfo), target :: _GENERIC_REF_typeInfo(refId)
!!  ! skip expansion of operators for casting procedures, because they
!!  !   don't work and lead to strange error messages
!!  ! use macros ref_to_proc and proc_to_ref instead
!!
!!# define PrivateProcReference( refId, baseType ) \
!!  _GENERIC_REF_ProcReference(private,refId,baseType)
!!
!!# define PublicProcReference( refId, baseType ) \
!!  _GENERIC_REF_ProcReference(public,refId,baseType)
!!
!!# define ProcReference  PrivateProcReference
!!
!!
!!
!!
!!! Define code fragments needed to transform a reference into a type neutral byte array.
!!! The following defines three options to achieve this.
!!! - using a union       : the most efficient way, but often not supported by fortran compilers as not in the standard.
!!! - using a cray pointer: still quite efficient and usually supported by fortran compilers, but also not standard.
!!! - using transfer      : not very effective but widely supported since its part of the standard fortran 95
!!
!!# define _GENERIC_REF_frag_declare_union( refId )                            \
!!    type CastMold                                                           ;\
!!      union                                                                 ;\
!!        map; type (refId)                                 :: wrap; end map  ;\
!!        map; character(len=storage_size(refId(null()))/8) :: enc;  end map  ;\
!!      end union                                                             ;\
!!    end type                                                                ;\
!!    type (CastMold) :: mold
!!
!!# define _GENERIC_REF_frag_encode_by_union( tgt, src )  \
!!    mold%wrap%ptr => src                               ;\
!!    tgt = mold%enc
!!
!!# define _GENERIC_REF_frag_decode_by_union( tgt, src )  \
!!    mold%enc = src%hook                                ;\
!!    tgt => mold%wrap%ptr
!!
!!
!!
!!# define _GENERIC_REF_frag_declare_crayPtr( refId )  \
!!    type (refId)                        :: wrap     ;\
!!    character(len=storage_size(wrap)/8) :: enc      ;\
!!    pointer (cray_link__, enc)                      ;\
!!    cray_link__ = loc(wrap)
!!
!!# define _GENERIC_REF_frag_encode_by_crayPtr( tgt, src )  \
!!    wrap%ptr => src                                      ;\
!!    tgt = enc
!!
!!# define _GENERIC_REF_frag_decode_by_crayPtr( tgt, src )  \
!!    enc = src%ref                                        ;\
!!    tgt => wrap%ptr
!!    
!!
!!
!!# define _GENERIC_REF_frag_declare_transfer( refId )  \
!!    type (refId) :: wrap
!!
!!# define _GENERIC_REF_frag_encode_by_transfer( tgt, src )  \
!!    wrap%ptr => src                                       ;\
!!    call GenericRef_prepare( tgt, storage_size(wrap)/8 )  ;\
!!    tgt%ref = transfer( wrap, tgt%ref )
!!
!!# define _GENERIC_REF_frag_decode_by_transfer( tgt, src )  \
!!    wrap = transfer( src%ref, wrap )                      ;\
!!    tgt  => wrap%ptr
!!
!!
!!! define code fragments for encoding / decoding pointers according
!!!   to the compiler's capabilities
!!
!!# if   defined __HAS_UNION
!!#   define _GENERIC_REF_frag_declare      _GENERIC_REF_frag_declare_union
!!#   define _GENERIC_REF_frag_encode       _GENERIC_REF_frag_encode_by_union
!!#   define _GENERIC_REF_frag_decode       _GENERIC_REF_frag_decode_by_union
!!
!!# elif defined __HAS_CRAY_PTR
!!#   define _GENERIC_REF_frag_declare      _GENERIC_REF_frag_declare_crayPtr
!!#   define _GENERIC_REF_frag_encode       _GENERIC_REF_frag_encode_by_crayPtr
!!#   define _GENERIC_REF_frag_decode       _GENERIC_REF_frag_decode_by_crayPtr
!!
!!# else
!!#   define _GENERIC_REF_frag_declare      _GENERIC_REF_frag_declare_transfer
!!#   define _GENERIC_REF_frag_encode       _GENERIC_REF_frag_encode_by_transfer
!!#   define _GENERIC_REF_frag_decode       _GENERIC_REF_frag_decode_by_transfer
!!
!!# endif
!!
!!
!!
!!
!!# define implementRefEncoding( refId, baseType, dimType, inspectF, cloneF, deleteF )      \
!!  function _GENERIC_REF_encode(refId)( val ) result(res)                                 ;\
!!    use type_info                                                                        ;\
!!    baseType dimType(), target, intent(in) :: val                                        ;\
!!    type (GenericRef)                    :: res                                          ;\
!!    procedure(), pointer :: _GENERIC_REF_func_none => null()                             ;\
!!    _GENERIC_REF_frag_declare(refId)                                                     ;\
!!    \
!!    _GENERIC_REF_frag_encode(res,val)                                                    ;\
!!    if (.not. _GENERIC_REF_typeInfo(refId)%initialized) then                             ;\
!!      call TypeInfo_init( _GENERIC_REF_typeInfo(refId), _str(refId), _str(baseType),      \
!!            storage_size(val)/8, size(shape(val)),                                        \
!!            _GENERIC_REF_func_inspect = inspectF(inspect,refId),                          \
!!            _GENERIC_REF_func_clone   = cloneF(clone,refId),                              \
!!            _GENERIC_REF_func_delete  = deleteF(delete,refId)                             \
!!          )                                                                              ;\
!!    end if                                                                               ;\
!!    res%typeInfo => _GENERIC_REF_typeInfo(refId)                                         ;\
!!  end function
!!
!!
!!
!!# define implementRefDecoding( refId, baseType, dimType )   \
!!  function _GENERIC_REF_decode(refId)( val ) result(res)   ;\
!!    type (GenericRef),  intent(in) :: val                  ;\
!!    baseType dimType(),    pointer :: res                  ;\
!!    _GENERIC_REF_frag_declare(refId)                       ;\
!!    \
!!    _GENERIC_REF_frag_decode(res,val)                      ;\
!!  end function
!!
!!
!!
!!# define implementRefInspector( refId )                            \
!!  subroutine _GENERIC_REF_inspect(refId)( val, res )              ;\
!!    type (GenericRef) :: val                                      ;\
!!    type (ShapeInfo)  :: res                                      ;\
!!    \
!!    res%rank = .rank. val                                         ;\
!!    res%sizes(:res%rank) = shape(_GENERIC_REF_decode(refId)(val)) ;\
!!  end subroutine
!!
!!
!!
!!# define _GENERIC_REF_frag_clone_default         \
!!    allocate( tgt, source=src )
!!
!!# define _GENERIC_REF_frag_clone_procedure(func) \
!!    tgt => func( src )
!!
!!# define implementRefCloning( refId, baseType, dimType, cloneType )   \
!!  subroutine _GENERIC_REF_clone(refId)( val, res )                   ;\
!!    type (GenericRef)           :: val                               ;\
!!    type (GenericRef)           :: res                               ;\
!!    baseType dimType(), pointer :: src, tgt => null()                ;\
!!    \
!!    src => _GENERIC_REF_decode(refId)(val)                           ;\
!!    tgt => cloneType(alloc,ptr)( src )                               ;\
!!    res = _GENERIC_REF_encode(refId)( tgt )                          ;\
!!    contains                                                         ;\
!!    function GenericRef_alloc_ptr( src )                             ;\
!!      baseType dimType(), pointer :: src, ptr, alloc                 ;\
!!      allocate( ptr, source=src )                                    ;\
!!      alloc => ptr                                                   ;\
!!    end function                                                     ;\
!!  end subroutine
!!
!!
!!
!!# define implementProcEncoding( refId, baseType )                                          \
!!  function _GENERIC_REF_encode(refId)( val ) result(res)                                  ;\
!!    use type_info                                                                         ;\
!!    procedure(baseType) :: val                                                            ;\
!!    type (GenericRef)   :: res                                                            ;\
!!    _GENERIC_REF_frag_declare(refId)                                                      ;\
!!    \
!!    _GENERIC_REF_frag_encode(res,val)                                                     ;\
!!    if (.not. _GENERIC_REF_typeInfo(refId)%initialized) then                              ;\
!!      call TypeInfo_init( _GENERIC_REF_typeInfo(refId), _str(refId), _str(baseType), 0 )  ;\
!!    end if                                                                                ;\
!!    res%typeInfo => _GENERIC_REF_typeInfo(refId)                                          ;\
!!  end function
!!
!!
!!
!!# define implementProcDecoding( refId, baseType )          \
!!  function _GENERIC_REF_decode(refId)( val ) result(res)  ;\
!!    type (GenericRef), intent(in) :: val                  ;\
!!    procedure(baseType),  pointer :: res                  ;\
!!    _GENERIC_REF_frag_declare(refId)                      ;\
!!    \
!!    _GENERIC_REF_frag_decode(res,val)                     ;\
!!  end function
!!
!!
!!
!!# define implementProcCloning( refId, baseType, dimType, cloneType ) \
!!  ! not needed
!!
!!
!!! We might need helper macros for converting between procedures to references ...
!!
!!# define proc_to_ref( _proc, _procRefId ) \
!!    _GENERIC_REF_encode(_procRefId)( _proc )
!!
!!# define ref_to_proc( _ref, _procRefId ) \
!!    _GENERIC_REF_decode(_procRefId)( _ref )



!_TypeReference(Int,integer*4,_scalar)
!_TypeReference(Array,integer*4,_rank_4)
!_ProcReference(xFunc,simpleCall)

!implementRefEncoding(Int,integer*4,_scalar,_default,_none,_procedure(myfunc))
!_implementRefEncoding(Array,integer*4,_rank_4,_default,_default,_default)

!implementRefDecoding(Int,integer*4,_rank_4)
!implementRefCloning(Int,integer*4,_rank_4,_procedure(mycloner))

!implementRefCloning(Int,integer*4,_rank_4,_default)

!_implementRefInspector(Int)

!_implementProcEncoding(xFunc,simpleCall)

!_implementProcDecoding(xFunc,simpleCall)


