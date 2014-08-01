
#include "adt/var_item.fpp"
#include "ref_status.fpp"

module var_item
  use generic_ref
  use type_info
  use dynamic_string
  use iso_c_binding
  implicit none
  private

# define _Table_nonPrimitive_types_ \
    _np_Type(string,  type(DynamicString_t), assignProc => ds_assign_ds, deleteProc => ds_delete) \
    _np_Type(gref,    type(GenericRef),      assignProc => gr_assign_gr, deleteProc => gr_delete) \
    _np_Type(VarItem, type(VarItem),         assignProc => vi_assign_vi, deleteProc => vi_delete)


! declare dummy variables - used to determine each type's storage_size ...
# define _initType_(typeId, baseType)   baseType :: _paste(typeId,_var);
    _Table_varItem_types_
# undef _initType_


# define _initType_(typeId, baseType) \
    , storage_size(_paste(typeId,_var))

  integer*4, parameter :: maxBytes = max(0 _Table_varItem_types_)/8
# undef _initType_


  type, public :: VarItem_t
    private
    integer*1               :: data(maxBytes) = 0
    type(TypeInfo), pointer :: typeInfo => null()
  end type

  ! declare VarItem(<type>) interface ...
# define _initType_(typeId, baseType) \
    , _paste(vi_from_,typeId)

  interface VarItem
    module procedure vi_from_vi         &
                   , vi_from_charString &
                     _Table_varItem_types_
  end interface
# undef _initType_


  ! declare operators (.<typeId>.) for getting type pointers
  ! and declare them public
# define _initType_(typeId, baseType) \
    interface typeId; module procedure _paste(vi_get_,typeId); end interface; \
    public :: typeId;
    _Table_varItem_types_
# undef _initType_


  ! declare assignment interface
# define _initType_(typeId, baseType) \
    , _paste(typeId,_assign_vi), _paste(vi_assign_,typeId)

  interface assignment(=)
    module procedure vi_assign_vi         &
                   , vi_assign_charString &
                     _Table_varItem_types_
  end interface
# undef _initType_


  ! declare TypeInfo variables, one for each type
  ! and set flag that they need to be initialized ...
# define _initType_(typeId, baseType) \
    type(TypeInfo), target :: _paste(vi_type_,typeId);

    _Table_varItem_types_
# undef _initType_
    type(TypeInfo), target :: vi_type_VarItem
    logical                :: is_initialized = .false.
  

  ! declare interfaces public

    interface is_valid   ; module procedure vi_is_valid   ; end interface
    interface typeinfo_of; module procedure vi_typeinfo_of; end interface
    interface delete     ; module procedure vi_delete     ; end interface

    public :: VarItem
    public :: is_valid
    public :: typeinfo_of
    public :: delete
    public :: assignment (=)
    public :: vi_type_VarItem

  ! declare predicate functions public ...
# define _initType_(typeId, baseType) \
    public :: _paste(is_,typeId);

    _Table_varItem_types_
# undef _initType_

  
!-----------------
  contains
!-----------------


  subroutine vi_initilize_module()
    ! call type initialization for each type ...
#   define _initType_(typeId, baseType) \
      call init_TypeInfo( _paste(vi_type_,typeId), _str(typeId), _str(baseType) \
                          , int(storage_size(_paste(typeId,_var)),4), 0 );
      _Table_varItem_types_
#   undef _initType_

    ! for all non-primitive types assign procedure pointers for assignment and deletion
#   define _np_Type(typeId, baseType, assignment, delete )  \
      _paste(vi_type_,typeId)%assignment                   ;\
      _paste(vi_type_,typeId)%delete                       ;\
      
      _Table_nonPrimitive_types_
#   undef _np_Type
    is_initialized = .true.
  end subroutine


# define _implementConstructor_(typeId, baseType)          \
    function _paste(vi_from_,typeId)( val ) result(res)   ;\
      baseType,    intent(in) :: val                      ;\
      baseType,       pointer :: ptr                      ;\
      type(VarItem_t), target :: res                      ;\
      call vi_reshape( res, _paste(vi_type_,typeId), 0 )  ;\
      call c_f_pointer( c_loc(res%data(1)), ptr )         ;\
      ptr = val                                           ;\
    end function

  _implementConstructor_(bool,     logical)
  _implementConstructor_(byte,     integer*1)
  _implementConstructor_(shortInt, integer*2)
  _implementConstructor_(int32,    integer*4)
  _implementConstructor_(longInt,  integer*8)
  _implementConstructor_(float,    real*4)
  _implementConstructor_(double,   real*8)
  _implementConstructor_(longDbl,  real*16)
  _implementConstructor_(cplx,     complex*8)
  _implementConstructor_(dblCplx,  complex*16)
  _implementConstructor_(quadCplx, complex*32)
  _implementConstructor_(cptr,     type(c_ptr))
  _implementConstructor_(string,   type(DynamicString_t))
  _implementConstructor_(gref,     type(GenericRef))


  function vi_from_charString( val ) result(res)
    character(len=*),   intent(in) :: val
    type(DynamicString_t), pointer :: ptr
    type(VarItem_t),        target :: res
    call vi_reshape( res, vi_type_string, 0 )
    call c_f_pointer( c_loc(res%data(1)), ptr )
    ptr = val
  end function


  function vi_from_vi( val ) result(res)
    type(VarItem_t), intent(in) :: val
    type(VarItem_t)             :: res

    call vi_reshape( res, val%typeInfo, 0 )
    if (associated( res%typeInfo )) then
      select case (associated( res%typeInfo%assignProc ))
        case (.true.) ; call res%typeInfo%assignProc( res%data, val%data )
        case (.false.); res%data = val%data
      end select
    end if
  end function


# define _implementGetter_(typeId, baseType)               \
    function _paste(vi_get_,typeId)( self ) result(res)   ;\
      type(VarItem_t), target, intent(in) :: self         ;\
      baseType,                   pointer :: res          ;\
      call vi_reshape( self, _paste(vi_type_,typeId), 1 ) ;\
      call c_f_pointer( c_loc(self%data(1)), res )        ;\
    end function

  _implementGetter_(bool,     logical)
  _implementGetter_(byte,     integer*1)
  _implementGetter_(shortInt, integer*2)
  _implementGetter_(int32,    integer*4)
  _implementGetter_(longInt,  integer*8)
  _implementGetter_(float,    real*4)
  _implementGetter_(double,   real*8)
  _implementGetter_(longDbl,  real*16)
  _implementGetter_(cplx,     complex*8)
  _implementGetter_(dblCplx,  complex*16)
  _implementGetter_(quadCplx, complex*32)
  _implementGetter_(cptr,     type(c_ptr))
  _implementGetter_(string,   type(DynamicString_t))
  _implementGetter_(gref,     type(GenericRef))


# define _implementSetter_(typeId, baseType)              \
    subroutine _paste(vi_assign_,typeId)( lhs, rhs )     ;\
      type(VarItem_t), target, intent(inout) :: lhs      ;\
      baseType,                   intent(in) :: rhs      ;\
      baseType,                      pointer :: ptr      ;\
      call vi_reshape( lhs, _paste(vi_type_,typeId), 1 ) ;\
      call c_f_pointer( c_loc(lhs%data(1)), ptr )        ;\
      ptr = rhs                                          ;\
    end subroutine

  _implementSetter_(bool,     logical)
  _implementSetter_(byte,     integer*1)
  _implementSetter_(shortInt, integer*2)
  _implementSetter_(int32,    integer*4)
  _implementSetter_(longInt,  integer*8)
  _implementSetter_(float,    real*4)
  _implementSetter_(double,   real*8)
  _implementSetter_(longDbl,  real*16)
  _implementSetter_(cplx,     complex*8)
  _implementSetter_(dblCplx,  complex*16)
  _implementSetter_(quadCplx, complex*32)
  _implementSetter_(cptr,     type(c_ptr))
  _implementSetter_(string,   type(DynamicString_t))
  _implementSetter_(gref,     type(GenericRef))


  subroutine vi_assign_charString( lhs, rhs )
    type(VarItem_t), target, intent(inout) :: lhs
    character(len=*),           intent(in) :: rhs
    type(DynamicString_t),         pointer :: ptr
    call vi_reshape( lhs, vi_type_string, 1 )
    call c_f_pointer( c_loc(lhs%data(1)), ptr )
    ptr = rhs
  end subroutine


  subroutine vi_assign_vi( lhs, rhs )
    type(VarItem_t), intent(inout) :: lhs
    type(VarItem_t)                :: rhs

    ! can't prevent self assignment ... (since fortran gives us a shallow copy of rhs)
    ! so in case - just do it and let sensitive types handle it themselves.
    call vi_reshape( lhs, rhs%typeInfo, 1 ) !< lhs' get's always hard on assignment!
    if (associated( lhs%typeInfo )) then
      select case (associated( lhs%typeInfo%assignProc ))
        case (.true.) ; call lhs%typeInfo%assignProc( lhs%data, rhs%data )
        case (.false.); lhs%data = rhs%data
      end select
    end if
  end subroutine


# define _implementAssignTo_(typeId, baseType)                       \
    subroutine _paste(typeId,_assign_vi)( lhs, rhs )                ;\
      baseType,     intent(inout) :: lhs                            ;\
      type(VarItem_t), intent(in) :: rhs                            ;\
      if (associated( rhs%typeInfo, _paste(vi_type_,typeId) )) then ;\
        lhs = typeId(rhs)                                           ;\
      end if                                                        ;\
    end subroutine

  _implementAssignTo_(bool,     logical)
  _implementAssignTo_(byte,     integer*1)
  _implementAssignTo_(shortInt, integer*2)
  _implementAssignTo_(int32,    integer*4)
  _implementAssignTo_(longInt,  integer*8)
  _implementAssignTo_(float,    real*4)
  _implementAssignTo_(double,   real*8)
  _implementAssignTo_(longDbl,  real*16)
  _implementAssignTo_(cplx,     complex*8)
  _implementAssignTo_(dblCplx,  complex*16)
  _implementAssignTo_(quadCplx, complex*32)
  _implementAssignTo_(cptr,     type(c_ptr))
  _implementAssignTo_(string,   type(DynamicString_t))
  _implementAssignTo_(gref,     type(GenericRef))


# define _implementPredicate_(typeId, baseType)                   \
    logical function _paste(is_,typeId)( self ) result(res)      ;\
      type(VarItem_t), intent(in) :: self                        ;\
      res = associated( self%typeInfo, _paste(vi_type_,typeId) ) ;\
    end function

  _implementPredicate_(bool,     logical)
  _implementPredicate_(byte,     integer*1)
  _implementPredicate_(shortInt, integer*2)
  _implementPredicate_(int32,    integer*4)
  _implementPredicate_(longInt,  integer*8)
  _implementPredicate_(float,    real*4)
  _implementPredicate_(double,   real*8)
  _implementPredicate_(longDbl,  real*16)
  _implementPredicate_(cplx,     complex*8)
  _implementPredicate_(dblCplx,  complex*16)
  _implementPredicate_(quadCplx, complex*32)
  _implementPredicate_(cptr,     type(c_ptr))
  _implementPredicate_(string,   type(DynamicString_t))
  _implementPredicate_(gref,     type(GenericRef))


  logical function vi_is_valid( self ) result(res)
    type(VarItem_t), intent(in) :: self
    res = associated( self%typeInfo )
  end function


  function vi_typeinfo_of( self ) result(res)
    type(VarItem_t), intent(in) :: self
    type(TypeInfo),     pointer :: res

    select case (associated( self%typeInfo ))
      case (.true.) ; res => self%typeInfo
      case (.false.); res => type_void
    end select
  end function


  subroutine vi_delete( self )
    type(VarItem_t) :: self
    
    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%deleteProc )) &
        call self%typeInfo%deleteProc( self%data )
      self%typeInfo => null()
    end if
  end subroutine
 

  subroutine vi_reshape( self, new_typeInfo, hardness )
    type(VarItem_t)                    :: self
    type(TypeInfo), target, intent(in) :: new_typeInfo 
    integer,                intent(in) :: hardness

    if (.not. associated( self%typeInfo, new_typeInfo )) then
      ! Important: check for initialization - even with associated typeInfo,
      !   because recursively loaded DLLs get separated data segments!
      !   Thus, it's possible to come here with self%typeInfo pointing to
      !   another data segment, but the current is not initialized yet!
      if (.not. is_initialized) &
        call vi_initilize_module()

      if (associated( self%typeInfo )) then
        if (associated( self%typeInfo%deleteProc )) &
          call self%typeInfo%deleteProc( self%data )
      end if
      ! IMPORTANT: the new type might hold a RefStatus (by convention at the structure begin).
      !   We initialize it according to given hardness
      _ref_init( self%data, hardness )
      self%typeInfo  => new_typeInfo
    end if
  end subroutine

end module var_item

