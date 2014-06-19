
#include "adt/ppUtil.xpp"

module generic_ref
  use dynamic_string
  use type_info
  use iso_c_binding
  implicit none
  private

  type, public :: GenericRef
    private
    type (DynamicString)     :: ref
    type (TypeInfo), pointer :: typeInfo => null()
  end type


  ! declare public interfaces 

  public :: assignment(=)
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
      if (associated( self%typeInfo%shapeFunc )) &
        call self%typeInfo%shapeFunc( self, res, self%typeInfo%rank )
        return
    end if
    res = 0
  end function


  function gr_clone( self ) result(res)
    type (GenericRef), intent(in) :: self
    type (GenericRef)             :: res

    res%ref = VolatileString()
    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%cloneFunc )) &
        call self%typeInfo%cloneFunc( self, res )
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
          if (associated( self%typeInfo%deleteFunc )) &
            call self%typeInfo%deleteFunc( wrap%ptr )
        end if
        deallocate( wrap%ptr )
      end if
    end if
    call delete( self )
  end subroutine

end module


!--------------------------------------------------------------
!  testing
!--------------------------------------------------------------

module encoders
  use generic_ref
  use type_info
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
      call TypeInfo_init( _typeInfo, _str(_typeId), _str(_baseType), &
                          storage_size(val)/8, size(shape(val)), &
                          ! encoder + decoder
                          cloneFunc = _cloner, &
                          shapeFunc = _inspect )
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
    type (GenericRef), intent(in) :: val
    type (GenericRef)             :: res
    _baseType _rank,      pointer :: src, tgt => null()
    src => _decoder( val )
    !?????????????
  end subroutine


end module


!##################################################################################################
#ifdef TEST

program testinger
  use generic_ref
  use encoders
  use iso_c_binding
  implicit none

  integer*4, dimension(3,5)          :: intArray
  integer*4, dimension(:,:), pointer :: ptr => null()

  type (c_ptr)      :: cpointer
  type (GenericRef) :: ref

  ref = GenericRef( intArray )
  ptr => IntPtr(ref)
  ptr = 42
  cpointer = cptr(ref)

  allocate( ptr(4,4) )
  ref = GenericRef( ptr )

  ptr => null()
  ptr => IntPtr(ref)

  print *, shape(ref)

  call free(ref)
  call free(ref)
  call delete( ref )

end

#endif

