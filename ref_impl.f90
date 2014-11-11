
#include "adt/ref_status.fpp"
#include "adt/itfUtil.fpp"

module adt_ref__
  use adt_ref, only: RefEncoding_t
  use adt_basestring
  use adt_typeinfo
  use iso_c_binding

# define Ref_t    Ref_t__impl__

  type, public :: Ref_t
    type(TypeInfo_t), pointer :: typeInfo => null()
    type(BaseString_t)        :: ref_str
    _RefStatus                :: refstat = _ref_HardLent
  end type


  interface
    pure &
    function ref_rank_c( self ) result(res)
      import Ref_t
      type(Ref_t), intent(in) :: self
      integer                 :: res
    end function

    pure &
    function ref_shape( self ) result(res)
      import Ref_t
      type(Ref_t), intent(in) :: self
      integer                 :: res(ref_rank_c(self))
    end function

    function ref_clone( self ) result(res)
      import Ref_t
      type(Ref_t), intent(in) :: self
      type(Ref_t)             :: res
    end function

    subroutine ref_assign_ref_c( lhs, rhs )
      import Ref_t
      type(Ref_t), intent(inout) :: lhs
      type(Ref_t),    intent(in) :: rhs
    end subroutine

    function ref_get_typereference( self ) result(res)
      import Ref_t, c_ptr
      type(Ref_t), intent(in) :: self
      type(c_ptr)             :: res
    end function

    recursive &
    subroutine ref_free_c( self )
      import Ref_t
      type(Ref_t) :: self
    end subroutine
  end interface
end module


!_PROC_EXPORT(ref_object_size_c)
  integer(kind=4) &
  function ref_object_size_c() result(res)
    use adt_ref__; implicit none
    type (Ref_t) :: tmp
    res = storage_size(tmp) / 8
  end function


!_PROC_EXPORT(ref_init_by_proto_c)
!_ARG_REFERENCE2(self, proto)
  subroutine ref_init_by_proto_c( self, has_proto, proto )
    use adt_ref__
    implicit none
    type(Ref_t), intent(inout) :: self
    integer,     intent(in)    :: has_proto
    type(Ref_t), intent(in)    :: proto
    
    if (has_proto /= 0) then;
      _ref_init( self%refstat, _ref_hardness(proto%refstat) )
      call basestring_init_by_proto_c( self%ref_str, 1, proto%ref_str )
    else;
      self%refstat = _ref_HardLent
      call basestring_init_by_proto_c( self%ref_str, 0, self%ref_str )
    end if
    self%typeInfo => null()
  end subroutine


!_PROC_EXPORT(ref_assign_ref_c)
!_ARG_REFERENCE2(lhs, rhs)
  subroutine ref_assign_ref_c( lhs, rhs )
    use adt_ref__, only: Ref_t, ref_free_c, basestring_assign_basestring_c
    implicit none
    type(Ref_t), intent(inout) :: lhs
    type(Ref_t),    intent(in) :: rhs

    if (.not. associated(lhs%ref_str%ptr, rhs%ref_str%ptr)) then
      call ref_free_c( lhs )
      call basestring_assign_basestring_c( lhs%ref_str, rhs%ref_str )
      lhs%typeInfo => rhs%typeInfo

      if (_ref_isWeakMine( rhs%refstat )) &
        _ref_setMine( lhs%refstat, 1 )
    end if
  end subroutine


!_PROC_EXPORT(ref_assign_encoding)
!_ARG_REFERENCE2(lhs, rhs)
  subroutine ref_assign_encoding( lhs, rhs )
    use adt_ref__
    implicit none
    type(Ref_t),              intent(inout) :: lhs
    type(RefEncoding_t), target, intent(in) :: rhs(:)
    integer(kind=4),              parameter :: size_typeInfo = storage_size(TypeInfo_ptr_t(null())) / 8
    integer(kind=4),              parameter :: size_encoding = storage_size(RefEncoding_t(null())) / 8
    character(len=1), dimension(:), pointer :: stream
    type(TypeInfo_ptr_t),           pointer :: typeInfo
    type(c_ptr)                             :: encoding
    
    call ref_free_c( lhs )
    encoding = c_loc(rhs(1))
    call c_f_pointer( encoding, typeInfo )
    call c_f_pointer( encoding, stream, (/ size(rhs) * size_encoding /) )
    call basestring_assign_buf( lhs%ref_str, stream(size_typeInfo + 1:) )
    lhs%typeInfo => typeInfo%ptr
  end subroutine


!_PROC_EXPORT(ref_get_typereference)
!_ARG_REFERENCE1(self)
  function ref_get_typereference( self ) result(res)
    use adt_ref__, only: Ref_t, c_ptr, basestring_cptr_c
    implicit none
    type(Ref_t), intent(in) :: self
    type(c_ptr)             :: res

    call basestring_cptr_c( res, self%ref_str )
  end function


!_PROC_EXPORT(ref_rank_c)
!_ARG_REFERENCE1(self)
  pure &
  function ref_rank_c( self ) result(res)
    use adt_ref__, only: Ref_t
    implicit none
    type(Ref_t), intent(in) :: self
    integer(kind=4)         :: res

    if (associated( self%typeInfo )) then
      res = self%typeInfo%typeSpecs%rank
    else
      res = 0
    end if
  end function


!_PROC_EXPORT(ref_shape)
!_ARG_REFERENCE1(self)
  pure &
  function ref_shape( self ) result(res)
    use adt_ref__, only: Ref_t, ref_rank_c
    implicit none
    type(Ref_t), intent(in) :: self
    integer                 :: res(ref_rank_c(self))

    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%shapeProc )) then
        call self%typeInfo%shapeProc( self, res, self%typeInfo%typeSpecs%rank )
        return
      end if
    end if
    res = 0
  end function


!_PROC_EXPORT(ref_get_shape_c)
!_ARG_REFERENCE3(self, buf, n)
  logical &
  function ref_get_shape_c( self, buf, n ) result(res)
    use adt_ref__
    implicit none
    type(Ref_t), intent(in) :: self
    integer(kind=c_size_t)  :: buf(n), n, rnk

    rnk = ref_rank_c(self)
    res = (n >= rnk)
    n   = rnk
    if (rnk > 0 .and. res) &
      buf(:n) = ref_shape( self )
  end function


!_PROC_EXPORT(ref_clone)
!_ARG_REFERENCE1(self)
  function ref_clone( self ) result(res)
    use adt_ref__, only: Ref_t, basestring_set_attribute, attribute_volatile
    implicit none
    type(Ref_t), intent(in) :: self
    type(Ref_t)             :: res

    call basestring_set_attribute( res%ref_str, attribute_volatile )
    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%cloneRefProc )) then
        call self%typeInfo%cloneRefProc( res, self )
        res%refstat = _ref_WeakMine
        return
      end if
    end if
    res = self
  end function


!_PROC_EXPORT(ref_clone_c)
!_ARG_REFERENCE1(self)
  subroutine ref_clone_c( res, self )
    use adt_ref__
    implicit none
    type(Ref_t), intent(inout) :: res
    type(Ref_t), intent(in)    :: self
    call ref_assign_ref_c( res, ref_clone( self ) )
  end subroutine


!_PROC_EXPORT(ref_cptr)
!_ARG_REFERENCE1(self)
  function ref_cptr( self ) result(res)
    use adt_ref__
    implicit none
    type(Ref_t), intent(in) :: self
    type(c_ptr)             :: res
    type(void_t),   pointer :: wrap

    res = ref_get_typereference(self)
    if (c_associated(res)) then
      call c_f_pointer( res, wrap )
      res = c_loc(wrap%ptr)
    end if
  end function


!_PROC_EXPORT(ref_cptr_c)
!_ARG_REFERENCE1(self)
  subroutine ref_cptr_c( res, self )
    use adt_ref__
    implicit none
    type(c_ptr), intent(inout) :: res
    type(Ref_t), intent(in)    :: self
    type(void_t),      pointer :: wrap

    res = ref_get_typereference(self)
    if (c_associated(res)) then
      call c_f_pointer( res, wrap )
      res = c_loc(wrap%ptr)
    end if
  end subroutine


!_PROC_EXPORT(ref_delete_c)
!_ARG_REFERENCE1(self)
  recursive &
  subroutine ref_delete_c( self )
    use adt_ref__
    implicit none
    type(Ref_t) :: self

    call ref_free_c( self )
    call basestring_delete_c( self%ref_str )
    self%typeInfo => null()
  end subroutine


!_PROC_EXPORT(ref_free_c)
!_ARG_REFERENCE1(self)
  recursive &
  subroutine ref_free_c( self )
    use adt_ref__, only: Ref_t, void_t, ref_get_typereference
    use iso_c_binding
    implicit none
    type(Ref_t)           :: self
    type(void_t), pointer :: wrap
    type(c_ptr)           :: ptr

    if (_ref_isMine( self%refstat )) then
      ptr = ref_get_typereference(self)
      if (c_associated( ptr )) then
        call c_f_pointer( ptr, wrap )

        if (associated( wrap%ptr )) then
          if (associated( self%typeInfo )) then
            if (associated( self%typeInfo%deleteProc )) &
              call self%typeInfo%deleteProc( wrap%ptr )
          end if
          deallocate( wrap%ptr )
        end if
      end if
      _ref_setMine( self%refstat, 0 )
    end if
  end subroutine


!_PROC_EXPORT(ref_dynamic_type)
!_ARG_REFERENCE1(self)
  function ref_dynamic_type( self ) result(res)
    use adt_ref__
    implicit none
    type(Ref_t), intent(in) :: self
    type(TypeInfo_t), pointer :: res

    if (associated( self%typeInfo )) then; res => self%typeInfo
                                     else; res => void_type()
    end if
  end function

  
!_PROC_EXPORT(ref_dynamic_type_c)
!_ARG_REFERENCE1(self)
  subroutine ref_dynamic_type_c( res, self )
    use adt_ref__
    implicit none
    type(TypeSpecs_t), intent(inout) :: res
    type(Ref_t),          intent(in) :: self
    type(TypeInfo_t),        pointer :: ptr

    if (associated( self%typeInfo )) then; ptr => self%typeInfo
                                     else; ptr => void_type()
    end if
    res = ptr%typeSpecs
  end subroutine

  
!_PROC_EXPORT(ref_bind_c)
!_ARG_REFERENCE1(self)
  subroutine ref_bind_c( self, do_bind )
    use adt_ref__
    implicit none
    type(Ref_t), intent(inout) :: self
    logical                    :: do_bind
    _ref_setMine( self%refstat, merge( 1, 0, do_bind ) )
  end subroutine

