
#include "adt/ppUtil.xpp"

module generic_ref
  use dynamic_string
  use iso_c_binding
  implicit none
  private


  type, public :: TypeInfo
    character(32)                :: typeId;     integer*2 :: typeId_term   = 0
    character(32)                :: baseType;   integer*2 :: baseType_term = 0
    integer                      :: byteSize    =  0
    integer                      :: rank        =  0
    logical                      :: initialized = .false.

    ! type specific subroutines called by generic interfaces 
    procedure(), nopass, pointer :: assignProc  => null()
    procedure(), nopass, pointer :: shapeProc   => null()
    procedure(), nopass, pointer :: cloneProc   => null()
    procedure(), nopass, pointer :: deleteProc  => null()
  end type


  type (TypeInfo), target :: ti_void = TypeInfo( "void", 0, "", 0, 0, 0, .true., &
                                                 null(), null(), null(), null() )


  type, public :: GenericRef
    private
    type (DynamicString)     :: ref
    type (TypeInfo), pointer :: typeInfo => null()
  end type


  ! declare public interfaces 

  public :: assignment(=)
  public :: gr_init_TypeInfo
  public :: gr_set_TypeReference
  public :: gr_get_TypeReference
  public :: rank
  public :: shape
  public :: clone
  public :: cptr
  public :: delete
  public :: free


  ! interface definitions

  interface assignment(=)
    module procedure gr_assign_gr
  end interface

  interface rank  ; module procedure gr_rank  ; end interface
  interface shape ; module procedure gr_shape ; end interface
  interface clone ; module procedure gr_clone ; end interface
  interface cptr  ; module procedure gr_cptr  ; end interface
  interface delete; module procedure gr_delete; end interface
  interface free  ; module procedure gr_free  ; end interface


  type, public :: voidRef ; integer, pointer :: ptr; end type

!-----------------
  contains
!-----------------

  !**
  ! gr_init_TypeInfo initializes TypeInfo structure.
  ! @param self        - the TypeInfo to initialize
  ! @param typeId      - the type's id string (e.g. double)
  ! @param baseType    - the type's base string (e.g. real*8)
  ! @param byteSize    - the storage size of the type in bytes (=> storage_size(type)/8)
  ! @param rank        - the rank of the type
  ! @param assignProc  - the procedure to assign a variable of this type to another
  ! @param shapeProc   - the procedure to inspect the shape of a type instance
  ! @param cloneProc   - the procedure to clone a type instance
  ! @param deleteProc  - the procedure to delete a variable
  !*
  !PROC_EXPORT_1REF( gr_init_TypeInfo, self )
  subroutine gr_init_TypeInfo( self, typeId, baseType, byteSize, rank, &
                               assignProc, shapeProc, cloneProc, deleteProc )
    type (TypeInfo),    intent(inout) :: self
    character(len=*),      intent(in) :: typeId, baseType
    integer,               intent(in) :: byteSize
    integer,               intent(in) :: rank
    procedure(),             optional :: assignProc, shapeProc, cloneProc, deleteProc

    self%typeId   = typeId;   self%typeId_term   = 0
    self%baseType = baseType; self%baseType_term = 0
    self%byteSize = byteSize
    self%rank     = rank

    ! pre-initialize optional arguments
    self%assignProc => null()
    self%shapeProc  => null()
    self%cloneProc  => null()
    self%deleteProc => null()

    if (present(assignProc)) self%assignProc => assignProc
    if (present(shapeProc))  self%shapeProc  => shapeProc
    if (present(cloneProc))  self%cloneProc  => cloneProc
    if (present(deleteProc)) self%deleteProc => deleteProc
    self%initialized = .true.
  end subroutine


!--------------------------------------------------------------
!   generic_ref
!--------------------------------------------------------------

  subroutine gr_assign_gr( lhs, rhs )
    type (GenericRef), intent(inout) :: lhs
    type (GenericRef),    intent(in) :: rhs

    lhs%ref      =  rhs%ref
    lhs%typeInfo => rhs%typeInfo
  end subroutine


  function gr_set_TypeReference( self, cptr, bits, ti ) result(needInit)
    type (GenericRef), intent(inout) :: self
    type (c_ptr),         intent(in) :: cptr
    integer*4,            intent(in) :: bits
    type (TypeInfo),          target :: ti
    character(len=bits/8),   pointer :: ptr
    logical                          :: needInit

    call c_f_pointer( cptr, ptr )
    self%ref = VolatileString()
    self%ref = ptr
    needInit = .not. ti%initialized
    self%typeInfo => ti
  end function


  function gr_get_TypeReference( self ) result(res)
    type (GenericRef), intent(in) :: self
    type (c_ptr)                  :: res
    res = cptr(self%ref)
  end function


  pure function gr_rank( self ) result(res)
    type (GenericRef), intent(in) :: self
    integer                       :: res

    if (associated( self%typeInfo )) then
      res = self%typeInfo%rank
    else
      res = 0
    end if
  end function


  pure function gr_shape( self ) result(res)
    type (GenericRef), intent(in) :: self
    integer                       :: res(rank(self))

    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%shapeProc )) &
        call self%typeInfo%shapeProc( self, res, self%typeInfo%rank )
        return
    end if
    res = 0
  end function


  function gr_clone( self ) result(res)
    type (GenericRef), intent(in) :: self
    type (GenericRef)             :: res

    res%ref = VolatileString()
    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%cloneProc )) &
        call self%typeInfo%cloneProc( self, res )
        return
    end if
    res = self
  end function


  function gr_cptr( self ) result(res)
    type (GenericRef), intent(in) :: self
    type (c_ptr)                  :: res
    type (voidRef),       pointer :: wrap

    res = gr_get_TypeReference(self)
    if (c_associated(res)) then
      call c_f_pointer( res, wrap )
      res = c_loc(wrap%ptr)
    end if
  end function


  subroutine gr_delete( self )
    type (GenericRef) :: self

    call delete( self%ref )
    self%typeInfo => null()
  end subroutine


  subroutine gr_free( self )
    type (GenericRef)       :: self
    type (voidRef), pointer :: wrap
    type (c_ptr)            :: cptr

    cptr = gr_get_TypeReference(self)
    if (c_associated( cptr )) then
      call c_f_pointer( cptr, wrap )

      if (associated( wrap%ptr )) then
        if (associated( self%typeInfo )) then
          if (associated( self%typeInfo%deleteProc )) &
            call self%typeInfo%deleteProc( wrap%ptr )
        end if
        deallocate( wrap%ptr )
      end if
    end if
    call delete( self )
  end subroutine

end module


!---------------------------------------------------------------
!  module encoders represents the user code needed for using
!    GenericRef with arbitrary types.
!  Creating this code module should be automated by either
!    using the preprocessor (if possible) or some script.
!---------------------------------------------------------------

module encoders
  use generic_ref
  implicit none

# define _scalar()
# define _rank_0()                       _scalar()
# define _rank_1()                       , dimension(:)
# define _rank_2()                       , dimension(:,:)
# define _rank_3()                       , dimension(:,:,:)
# define _rank_4()                       , dimension(:,:,:,:)
# define _rank_5()                       , dimension(:,:,:,:,:)
# define _rank_6()                       , dimension(:,:,:,:,:,:)
# define _rank_7()                       , dimension(:,:,:,:,:,:,:)

# define _baseType  integer*4
# define _typeId    Int
# define _rank      _rank_2()

! derived ...
# define _typeInfo  _paste(TypeInfo_,_typeId)
# define _encoder   _paste(GenericRef_encode_,_typeId)
# define _decoder   _paste(GenericRef_decode_,_typeId)
# define _inspect   _paste(GenericRef_inspect_,_typeId)
# define _cloner    _paste(GenericRef_clone_,_typeId)



  type, public :: _typeId; _baseType _rank, pointer :: ptr; end type
  type (TypeInfo), target :: _typeInfo
  interface GenericRef;          module procedure _encoder; end interface
  interface _paste(_typeId,Ptr); module procedure _decoder; end interface

  contains

  function _encoder( val ) result(res)
    use iso_c_binding
    _baseType _rank, target, intent(in) :: val
    type (GenericRef)                   :: res
    type (_typeId),              target :: wrap

    wrap%ptr => val
    if (gr_set_TypeReference( res, c_loc(wrap), storage_size(wrap), _typeInfo )) &
      call gr_init_TypeInfo( _typeInfo, _str(_typeId), _str(_baseType), &
                          storage_size(val)/8, size(shape(val)), &
                          cloneProc = _cloner, &
                          shapeProc = _inspect )
  end function


  function _decoder( val ) result(res)
    use iso_c_binding
    type (GenericRef), intent(in) :: val
    _baseType _rank,      pointer :: res
    type (_typeId),       pointer :: wrap
    
    call c_f_pointer( gr_get_TypeReference(val), wrap )
    res => wrap%ptr
  end function


  subroutine _inspect( val, res, n )
    type (GenericRef), intent(in) :: val
    integer                       :: n
    integer                       :: res(n)
    res(:n) = shape( _decoder( val ) )
  end subroutine


  subroutine _cloner( val, res )
    use iso_c_binding
    type (GenericRef),           intent(in) :: val
    type (GenericRef)                       :: res
    _baseType _rank,                pointer :: src, tgt => null()
    character(len=1), dimension(:), pointer :: tmp

    src => _decoder( val )
    ! allocate target, and copy info
    allocate( tmp(product(shape(src)) * storage_size(src)/8) )
    call c_f_pointer( c_loc(tmp(1)), tgt, shape(src) ) !< CAUTION: do not provide shape for scalar types!
    tgt = src
    ! or
    ! call a function / subroutine to do it somehow ...
    res = _encoder( tgt )
  end subroutine

end module


!###########################################################################################
#ifdef TEST

program testinger
  use generic_ref
  use encoders
  use iso_c_binding
  implicit none

  integer*4, dimension(3,5), target  :: intArray
  integer*4, dimension(:,:), pointer :: ptr2d => null()
  integer*4, dimension(:)  , pointer :: ptr1d => null()
  integer*4,                 pointer :: ptr0d => null()

  type (c_ptr)      :: cpointer
  type (GenericRef) :: ref, ref2

  ref = GenericRef( intArray )
  ptr2d => IntPtr(ref)
  ptr2d = 42
  cpointer = cptr(ref)

  allocate( ptr2d(4,4) )
  ref = GenericRef( ptr2d )

  ptr2d => null()
  ptr2d => IntPtr(ref)

  ptr2d = 21

  print *, shape(ref)
  ref2 = clone(ref)
  print *, shape(ref2)
  print *, IntPtr(ref2)

  call free(ref)
  call free(ref2)
  call delete( ref )

  call do_clone( intArray, ptr2d )
  print *, ptr2d

  call do_clone2( 42, ptr0d )
  print *, ptr0d

  deallocate(ptr0d)
  deallocate(ptr2d)

  contains

# define alloc_size(x)  (product(shape(x)) * storage_size(x)/8)

  subroutine do_clone( src, tgt )
    integer*4, dimension(:,:),       target :: src
    integer*4, dimension(:,:),      pointer :: tgt
    character(len=1), dimension(:), pointer :: tmp

    allocate( tmp(alloc_size(src)) )
    call c_f_pointer( c_loc(tmp(1)), tgt, shape(src) )
    tgt = src
  end subroutine

  subroutine do_clone2( src, tgt )
    integer*4,                       target :: src
    integer*4,                      pointer :: tgt
    character(len=1), dimension(:), pointer :: tmp

    allocate( tmp(alloc_size(src)) )
    call c_f_pointer( c_loc(tmp(1)), tgt )
    tgt = src
  end subroutine

end

#endif

