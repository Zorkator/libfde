
#include "adt/ppUtil.xpp"

module var_item
  use generic_ref
  use dynamic_string
  use iso_c_binding
  implicit none
  private

# define _Table_varItem_types_ \
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


# define _Table_nonPrimitive_types_ \
    _np_Type(string, type(DynamicString), assignProc => ds_assign_ds, deleteProc => ds_delete ) \
    _np_Type(gref,   type(GenericRef),    assignProc => gr_assign_gr, deleteProc => gr_delete )


  type, public :: VarItem
    private
    integer*1                :: data(48) = 0
    type (TypeInfo), pointer :: typeInfo => null()
  end type

  ! declare VarItem(<type>) interface ...
# define _initType_(typeId, baseType) \
    , _paste(vi_from_,typeId)

  interface VarItem
    module procedure vi_from_vi &
                     _Table_varItem_types_
  end interface
# undef _initType_


  ! declare operators (.<typeId>.) for getting type pointers
  ! and declare them public
# define _initType_(typeId, baseType) \
    interface operator(.typeId.); procedure _paste(vi_get_,typeId); end interface; \
    public :: operator(.typeId.);

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
    type (TypeInfo), target :: _paste(vi_type_,typeId);

    _Table_varItem_types_
# undef _initType_
    logical :: is_initialized = .false.
  

  ! declare interfaces public

    interface delete; module procedure vi_delete; end interface

    public :: is_valid
    public :: typeinfo_of
    public :: delete
    public :: assignment (=)

  ! declare predicate functions public ...
# define _initType_(typeId, baseType) \
    public :: _paste(is_,typeId);

    _Table_varItem_types_
# undef _initType_

  
!-----------------
  contains
!-----------------


  subroutine vi_initilize_module()
    ! declare local dummy variables - used to determine each type's storage_size ...
#   define _initType_(typeId, baseType)   baseType :: _paste(typeId,_var);
      _Table_varItem_types_
#   undef _initType_

    ! call type initialization for each type ...
#   define _initType_(typeId, baseType) \
      call gr_init_TypeInfo( _paste(vi_type_,typeId), _str(typeId), _str(baseType) \
                             , storage_size(_paste(typeId,_var))/8, 0 );
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
      baseType,   intent(in) :: val                       ;\
      baseType,      pointer :: ptr                       ;\
      type (VarItem), target :: res                       ;\
      call vi_reshape( res, _paste(vi_type_,typeId), 0 )  ;\
      call c_f_pointer( c_loc(res%data(1)), ptr )         ;\
      ptr = val                                           ;\
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


  function vi_from_charString( val ) result(res)
    character(len=*),     intent(in) :: val
    type (DynamicString),    pointer :: ptr
    type (VarItem),           target :: res
    call vi_reshape( res, vi_type_string, 0 )
    call c_f_pointer( c_loc(res%data(1)), ptr )
    ptr = val
  end function


  function vi_from_vi( val ) result(res)
    type (VarItem), intent(in) :: val
    type (VarItem)             :: res

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
      type (VarItem), target, intent(in) :: self          ;\
      baseType,                  pointer :: res           ;\
      call vi_reshape( self, _paste(vi_type_,typeId), 1 ) ;\
      call c_f_pointer( c_loc(self%data(1)), res )        ;\
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


# define _implementSetter_(typeId, baseType)              \
    subroutine _paste(vi_assign_,typeId)( lhs, rhs )     ;\
      type (VarItem), target, intent(inout) :: lhs       ;\
      baseType,                  intent(in) :: rhs       ;\
      baseType,                     pointer :: ptr       ;\
      call vi_reshape( lhs, _paste(vi_type_,typeId), 1 ) ;\
      call c_f_pointer( c_loc(lhs%data(1)), ptr )        ;\
      ptr = rhs                                          ;\
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


  subroutine vi_assign_charString( lhs, rhs )
    type (VarItem), target, intent(inout) :: lhs
    character(len=*),          intent(in) :: rhs
    type (DynamicString),         pointer :: ptr
    call vi_reshape( lhs, vi_type_string, 1 )
    call c_f_pointer( c_loc(lhs%data(1)), ptr )
    ptr = rhs
  end subroutine


  subroutine vi_assign_vi( lhs, rhs )
    type (VarItem), intent(inout) :: lhs
    type (VarItem)                :: rhs

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
      case (.false.); res => type_void
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
 

  subroutine vi_reshape( self, new_typeInfo, hardness )
    type (VarItem)                      :: self
    type (TypeInfo), target, intent(in) :: new_typeInfo 
    integer,                 intent(in) :: hardness

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
      ! IMPORTANT: setting data to zero effectively marks any
      !   non-primitive types volatile, which is essential for constructors!
      ! Another reason for doing this is to eliminate any borrowed pointers
      !   that would cause trouble at another allocation.
      self%data     = 0 !! FIXME: need new_typeInfo%initProc here!!!!!
                        !! this is NOT ok for assignments as it overrides the
                        !! hardness of lhs!!!!!
      self%data(1)  = hardness
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

  type (VarItem)  :: v1, v2
  type (TypeInfo) :: ti
  integer*4       :: intvar
  type (DynamicString) :: ds
  type (GenericRef)    :: gr

  v1 = VarItem(345597)
  print *, .int.v1
  v1 = VarItem(34.55)
  print *, .float.v1

  print *, is_valid(v1)
  print *, is_float(v1)

  ti = typeinfo_of(v1)
  print *, ti%typeId

  v1 = 5.34
  v1 = DynamicString("testinger")
  v1 = 'bla & text'
  v1 = .ref.gr

  v2 = v1
  v1 = v1

  v1 = 42

  intvar = v1

  print *, storage_size(v1)/8
  print *, storage_size(ds)/8
  print *, storage_size(gr)/8

  call delete(ds)
  call delete(gr)
  call delete(v1)
  call delete(v2)


# define _initType_(typeId, baseType) \
    call _paste(test_,typeId)();

  _Table_varItem_types_
# undef _initType_



end

# define _nop(a)
# define _delete(a)   call delete(a)

# define _implementTest_(_typeId, _baseType, _finish) \
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
    _finish(val)                       ;\
  end subroutine

  _implementTest_(bool,     logical, _nop)
  _implementTest_(byte,     integer*1, _nop)
  _implementTest_(shortInt, integer*2, _nop)
  _implementTest_(int,      integer*4, _nop)
  _implementTest_(longInt,  integer*8, _nop)
  _implementTest_(float,    real*4, _nop)
  _implementTest_(double,   real*8, _nop)
  _implementTest_(longDbl,  real*16, _nop)
  _implementTest_(cplx,     complex*8, _nop)
  _implementTest_(dblCplx,  complex*16, _nop)
  _implementTest_(quadCplx, complex*32, _nop)
  _implementTest_(cptr,     type(c_ptr), _nop)
  _implementTest_(string,   type(DynamicString), _delete)
  _implementTest_(gref,   type(GenericRef), _delete)

#endif

