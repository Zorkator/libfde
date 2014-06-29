
#include "adt/ppUtil.xpp"

module var_item
  use generic_ref
  use dynamic_string
  use iso_c_binding
  implicit none
  private

# define _Table_typeInit_ \
    _initType_(bool,     logical)             \
    _initType_(byte,     integer*1)           \
    _initType_(shortInt, integer*2)           \
    _initType_(int,      integer*4)           \
    _initType_(longInt,  integer*8)           \
    _initType_(float,    real*4)              \
    _initType_(double,   real*8)              \
    _initType_(longDbl,  real*16)             \
    _initType_(cplx,     complex*8)           \
    _initType_(dblCplx,  complex*16)          \
    _initType_(quadCplx, complex*32)          \
    _initType_(cptr,     type(c_ptr))         \
    _initType_(string,   type(DynamicString)) \
    _initType_(gref,     type(GenericRef))


  type, public :: VarItem
    private
    integer*4                :: data(12) = 0
    type (TypeInfo), pointer :: typeInfo => null()
  end type

  ! declare VarItem(<type>) interface ...
# define _initType_(typeId, baseType) \
    module procedure _paste(vi_from_,typeId);

  interface VarItem
    _Table_typeInit_
  end interface
# undef _initType_


  ! declare operators (.<typeId>.) for getting type pointers
  ! and declare them public
# define _initType_(typeId, baseType) \
    interface operator(.typeId.); procedure _paste(vi_get_,typeId); end interface; \
    public :: operator(.typeId.);

    _Table_typeInit_
# undef _initType_


  ! declare TypeInfo variables, one for each type
  ! and set flag that they need to be initialized ...
# define _initType_(typeId, baseType) \
    type (TypeInfo), target :: _paste(vi_type_,typeId);

    _Table_typeInit_
# undef _initType_
    logical :: is_initialized = .false.

  
  ! declare predicate functions public ...
# define _initType_(typeId, baseType) \
    public :: _paste(is_,typeId);

    _Table_typeInit_
# undef _initType_

    public :: is_valid
    public :: typeinfo_of

    interface delete; module procedure vi_delete; end interface
    public :: delete
    public :: assignment (=)

  
  ! declare assignment interface
# define _initType_(typeId, baseType) \
    module procedure _paste(typeId,_assign_vi), _paste(vi_assign_,typeId);

  interface assignment(=)
    _Table_typeInit_
  end interface
# undef _initType_

!-----------------
  contains
!-----------------


  subroutine vi_initilize_module()
#   define _initType_(typeId, baseType)   baseType :: _paste(typeId,_var);
      _Table_typeInit_
#   undef _initType_

#   define _initType_(typeId, baseType) \
      call gr_init_TypeInfo( _paste(vi_type_,typeId), _str(typeId), _str(baseType) \
                             , storage_size(_paste(typeId,_var))/8, 0 );
      _Table_typeInit_
#   undef _initType_
    is_initialized = .true.
  end subroutine


# define _implementConstructor_(typeId, baseType)        \
    function _paste(vi_from_,typeId)( val ) result(res) ;\
      baseType,   intent(in) :: val                     ;\
      baseType,      pointer :: ptr                     ;\
      type (VarItem), target :: res                     ;\
      call vi_reshape( res, _paste(vi_type_,typeId) )   ;\
      call c_f_pointer( c_loc(res%data(1)), ptr )       ;\
      ptr = val                                         ;\
    end function

  _implementConstructor_(bool,     logical)
  _implementConstructor_(byte,     integer*1)
  _implementConstructor_(shortInt, integer*2)
  _implementConstructor_(int,      integer*4)
  _implementConstructor_(longInt,  integer*8)
  _implementConstructor_(float,    real*4)
  _implementConstructor_(double,   real*8)
  _implementConstructor_(longDbl,  real*16)
  _implementConstructor_(cplx,     complex*8)
  _implementConstructor_(dblCplx,  complex*16)
  _implementConstructor_(quadCplx, complex*32)
  _implementConstructor_(cptr,     type(c_ptr))
  _implementConstructor_(string,   type(DynamicString))
  _implementConstructor_(gref,     type(GenericRef))


# define _implementGetter_(typeId, baseType)             \
    function _paste(vi_get_,typeId)( self ) result(res) ;\
      type (VarItem), target, intent(in) :: self        ;\
      baseType,                  pointer :: res         ;\
      call vi_reshape( self, _paste(vi_type_,typeId) )  ;\
      call c_f_pointer( c_loc(self%data(1)), res )      ;\
    end function

  _implementGetter_(bool,     logical)
  _implementGetter_(byte,     integer*1)
  _implementGetter_(shortInt, integer*2)
  _implementGetter_(int,      integer*4)
  _implementGetter_(longInt,  integer*8)
  _implementGetter_(float,    real*4)
  _implementGetter_(double,   real*8)
  _implementGetter_(longDbl,  real*16)
  _implementGetter_(cplx,     complex*8)
  _implementGetter_(dblCplx,  complex*16)
  _implementGetter_(quadCplx, complex*32)
  _implementGetter_(cptr,     type(c_ptr))
  _implementGetter_(string,   type(DynamicString))
  _implementGetter_(gref,     type(GenericRef))


# define _implementSetter_(typeId, baseType)           \
    subroutine _paste(vi_assign_,typeId)( lhs, rhs )  ;\
      type (VarItem), target, intent(inout) :: lhs    ;\
      baseType,                  intent(in) :: rhs    ;\
      baseType,                     pointer :: ptr    ;\
      call vi_reshape( lhs, _paste(vi_type_,typeId) ) ;\
      call c_f_pointer( c_loc(lhs%data(1)), ptr )     ;\
      ptr = rhs                                       ;\
    end subroutine

  _implementSetter_(bool,     logical)
  _implementSetter_(byte,     integer*1)
  _implementSetter_(shortInt, integer*2)
  _implementSetter_(int,      integer*4)
  _implementSetter_(longInt,  integer*8)
  _implementSetter_(float,    real*4)
  _implementSetter_(double,   real*8)
  _implementSetter_(longDbl,  real*16)
  _implementSetter_(cplx,     complex*8)
  _implementSetter_(dblCplx,  complex*16)
  _implementSetter_(quadCplx, complex*32)
  _implementSetter_(cptr,     type(c_ptr))
  _implementSetter_(string,   type(DynamicString))
  _implementSetter_(gref,     type(GenericRef))


# define _implementAssignTo_(typeId, baseType)                       \
    subroutine _paste(typeId,_assign_vi)( lhs, rhs )                ;\
      baseType,    intent(inout) :: lhs                             ;\
      type (VarItem), intent(in) :: rhs                             ;\
      if (associated( rhs%typeInfo, _paste(vi_type_,typeId) )) then ;\
        lhs = .typeId. rhs                                          ;\
      end if                                                        ;\
    end subroutine

  _implementAssignTo_(bool,     logical)
  _implementAssignTo_(byte,     integer*1)
  _implementAssignTo_(shortInt, integer*2)
  _implementAssignTo_(int,      integer*4)
  _implementAssignTo_(longInt,  integer*8)
  _implementAssignTo_(float,    real*4)
  _implementAssignTo_(double,   real*8)
  _implementAssignTo_(longDbl,  real*16)
  _implementAssignTo_(cplx,     complex*8)
  _implementAssignTo_(dblCplx,  complex*16)
  _implementAssignTo_(quadCplx, complex*32)
  _implementAssignTo_(cptr,     type(c_ptr))
  _implementAssignTo_(string,   type(DynamicString))
  _implementAssignTo_(gref,     type(GenericRef))



# define _implementPredicate_(typeId, baseType)                   \
    logical function _paste(is_,typeId)( self ) result(res)      ;\
      type (VarItem), intent(in) :: self                         ;\
      res = associated( self%typeInfo, _paste(vi_type_,typeId) ) ;\
    end function

  _implementPredicate_(bool,     logical)
  _implementPredicate_(byte,     integer*1)
  _implementPredicate_(shortInt, integer*2)
  _implementPredicate_(int,      integer*4)
  _implementPredicate_(longInt,  integer*8)
  _implementPredicate_(float,    real*4)
  _implementPredicate_(double,   real*8)
  _implementPredicate_(longDbl,  real*16)
  _implementPredicate_(cplx,     complex*8)
  _implementPredicate_(dblCplx,  complex*16)
  _implementPredicate_(quadCplx, complex*32)
  _implementPredicate_(cptr,     type(c_ptr))
  _implementPredicate_(string,   type(DynamicString))
  _implementPredicate_(gref,     type(GenericRef))


  logical function is_valid( self ) result(res)
    type (VarItem), intent(in) :: self
    res = associated( self%typeInfo )
  end function


  function typeinfo_of( self ) result(res)
    type (VarItem), intent(in) :: self
    type (TypeInfo),   pointer :: res

    select case (associated( self%typeInfo ))
      case (.true.) ; res => self%typeInfo
      case (.false.); res => TypeInfo_void
    end select
  end function


  subroutine vi_delete( self )
    type (VarItem) :: self
    
    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%deleteProc )) &
        call self%typeInfo%deleteProc( self%data )
      self%typeInfo => null()
    end if
  end subroutine
 

  subroutine vi_reshape( self, new_typeInfo )
    type (VarItem)                      :: self
    type (TypeInfo), target, intent(in) :: new_typeInfo 

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
      self%data     = 0
      self%typeInfo => new_typeInfo
    end if
  end subroutine

end module var_item


!##################################################################################################
#ifdef TEST

program testinger
  use var_item
  use generic_ref
  use dynamic_string
  implicit none

  type (VarItem)  :: v
  type (TypeInfo) :: ti
  integer*4       :: intvar
  type (DynamicString) :: ds
  type (GenericRef)    :: gr

  v = VarItem(1)
  print *, .int.v
  v = VarItem(34.55)
  print *, .float.v

  print *, is_valid(v)
  print *, is_float(v)

  ti = typeinfo_of(v)
  print *, ti%typeId

  v = 5.34

  intvar = v

  print *, storage_size(v)/8
  print *, storage_size(ds)/8
  print *, storage_size(gr)/8

# define _initType_(typeId, baseType) \
    call _paste(test_,typeId)();

  _Table_typeInit_
# undef _initType_



end

# define _implementTest_(_typeId, _baseType) \
  subroutine _paste(test_,_typeId)()   ;\
    use var_item                       ;\
    use generic_ref                    ;\
    use dynamic_string                 ;\
    use iso_c_binding                  ;\
    _baseType                :: val    ;\
    _baseType,       pointer :: ptr    ;\
    type (VarItem)           :: vi     ;\
    type (TypeInfo), pointer :: ti     ;\
    vi  = VarItem(val)                 ;\
    vi  = val                          ;\
    ptr => ._typeId.vi                 ;\
    val = vi                           ;\
    ti  => typeinfo_of(vi)             ;\
    print *, ti%typeId, ti%baseType    ;\
    print *, _paste(is_,_typeId)( vi ) ;\
    print *, is_valid(vi)              ;\
    call delete( vi )                  ;\
    print *, is_valid(vi)              ;\
  end subroutine

  _implementTest_(bool,     logical)
  _implementTest_(byte,     integer*1)
  _implementTest_(shortInt, integer*2)
  _implementTest_(int,      integer*4)
  _implementTest_(longInt,  integer*8)
  _implementTest_(float,    real*4)
  _implementTest_(double,   real*8)
  _implementTest_(longDbl,  real*16)
  _implementTest_(cplx,     complex*8)
  _implementTest_(dblCplx,  complex*16)
  _implementTest_(quadCplx, complex*32)
  _implementTest_(cptr,     type(c_ptr))
  _implementTest_(string,   type(DynamicString))
  _implementTest_(gref,   type(GenericRef))

#endif

