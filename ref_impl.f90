
#include "adt/ref_status.fpp"
#include "adt/itfUtil.fpp"

module adt_ref__
  use adt_ref, only: Ref_t => Ref_t__impl__, RefEncoding_t
  use adt_basestring
  use adt_typeinfo
  use iso_c_binding

  interface
    pure function ref_rank( self ) result(res)
      import Ref_t
      type(Ref_t), intent(in) :: self
      integer                 :: res
    end function

    function ref_get_TypeReference( self ) result(res)
      import Ref_t, c_ptr
      type(Ref_t), intent(in) :: self
      type(c_ptr)             :: res
    end function
  end interface
end module

!_PROC_EXPORT(ref_init_by_proto)
  subroutine ref_init_by_proto( self, has_proto, proto )
    use adt_ref__
    implicit none
    type(Ref_t) :: self
    integer     :: has_proto
    type(Ref_t) :: proto
    
    if (has_proto /= 0) then;
      _ref_init( self%refstat, _ref_hardness(proto%refstat) )
      call basestring_init_by_proto( self%ref_str, 1, proto%ref_str )
    else;
      self%refstat = _ref_HardLent
      call basestring_init_by_proto( self%ref_str, 0, self%ref_str )
    end if
    self%typeInfo => null()
  end subroutine


!_PROC_EXPORT(ref_assign_ref)
  subroutine ref_assign_ref( lhs, rhs )
    use adt_ref__
    implicit none
    type(Ref_t), intent(inout) :: lhs
    type(Ref_t),    intent(in) :: rhs

    if (.not. associated(lhs%ref_str%ptr, rhs%ref_str%ptr)) then
      call ref_free( lhs )
      call basestring_assign_bs( lhs%ref_str, rhs%ref_str )
      lhs%typeInfo => rhs%typeInfo

      if (_ref_isWeakMine( rhs%refstat )) &
        _ref_setMine( lhs%refstat, 1 )
    end if
  end subroutine


!_PROC_EXPORT(ref_assign_encoding)
  subroutine ref_assign_encoding( lhs, rhs )
    use adt_ref__
    implicit none
    type(Ref_t),              intent(inout) :: lhs
    type(RefEncoding_t), target, intent(in) :: rhs(:)
    integer*4,                    parameter :: size_typeInfo = storage_size(TypeInfo_ptr_t(null())) / 8
    integer*4,                    parameter :: size_encoding = storage_size(RefEncoding_t(null())) / 8
    character(len=1), dimension(:), pointer :: stream
    type(TypeInfo_ptr_t),           pointer :: typeInfo
    type(c_ptr)                             :: encoding
    
    call ref_free( lhs )
    encoding = c_loc(rhs(1))
    call c_f_pointer( encoding, typeInfo )
    call c_f_pointer( encoding, stream, (/ size(rhs) * size_encoding /) )
    call basestring_assign_buf( lhs%ref_str, stream(size_typeInfo + 1:) )
    lhs%typeInfo => typeInfo%ptr
  end subroutine


!_PROC_EXPORT(ref_get_TypeReference)
  function ref_get_TypeReference( self ) result(res)
    use adt_ref__, only: Ref_t, c_ptr, basestring_cptr
    implicit none
    type(Ref_t), intent(in) :: self
    type(c_ptr)             :: res

    res = basestring_cptr( self%ref_str )
  end function


!_PROC_EXPORT(ref_rank)
  pure function ref_rank( self ) result(res)
    use adt_ref__, only: Ref_t
    implicit none
    type(Ref_t), intent(in) :: self
    integer                 :: res

    if (associated( self%typeInfo )) then
      res = self%typeInfo%rank
    else
      res = 0
    end if
  end function


!_PROC_EXPORT(ref_shape)
  pure function ref_shape( self ) result(res)
    use adt_ref__
    implicit none
    type(Ref_t), intent(in) :: self
    integer                 :: res(ref_rank(self))

    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%shapeProc )) then
        call self%typeInfo%shapeProc( self, res, self%typeInfo%rank )
        return
      end if
    end if
    res = 0
  end function


!_PROC_EXPORT(ref_clone)
  function ref_clone( self ) result(res)
    use adt_ref__
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


!_PROC_EXPORT(ref_cptr)
  function ref_cptr( self ) result(res)
    use adt_ref__
    implicit none
    type(Ref_t), intent(in) :: self
    type(c_ptr)             :: res
    type(void_t),   pointer :: wrap

    res = ref_get_TypeReference(self)
    if (c_associated(res)) then
      call c_f_pointer( res, wrap )
      res = c_loc(wrap%ptr)
    end if
  end function


!_PROC_EXPORT(ref_delete)
  recursive &
  subroutine ref_delete( self )
    use adt_ref__
    implicit none
    type(Ref_t) :: self

    call ref_free( self )
    call basestring_delete( self%ref_str )
    self%typeInfo => null()
  end subroutine


!_PROC_EXPORT(ref_free)
  recursive &
  subroutine ref_free( self )
    use adt_ref__
    implicit none
    type(Ref_t)           :: self
    type(void_t), pointer :: wrap
    type(c_ptr)           :: ptr

    if (_ref_isMine( self%refstat )) then
      ptr = ref_get_TypeReference(self)
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
  function ref_dynamic_type( self ) result(res)
    use adt_ref__
    implicit none
    type(Ref_t), intent(in) :: self
    type(TypeInfo_t), pointer :: res

    if (associated( self%typeInfo )) then; res => self%typeInfo
                                     else; res => type_void
    end if
  end function

  
