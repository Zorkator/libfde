
program testinger
  use generic_ref
  use type_references
  use base_types
  use iso_c_binding
  implicit none

  interface
    subroutine bla(); end subroutine
  end interface

  integer*4, dimension(3,5), target  :: intArray
  integer*4, dimension(:,:), pointer :: ptr2d => null()
  integer*4, dimension(:)  , pointer :: ptr1d => null()
  integer*4,                 pointer :: ptr0d => null()
  !procedure(bla), dimension(:), pointer :: procPtr #< not possible to create array of proc pointers!
  procedure(func),       pointer :: f  => null()
  procedure(simpleCall), pointer :: sc => null()
  type(Ding)                     :: dong
  integer*4                      :: i, j

  type(c_ptr)         :: cpointer
  type(GenericRef_t)  :: ref1, ref2, ref3
  complex*32          :: cplx
  complex*32, pointer :: cplx_ptr

  intArray = 34

  ref1 = ref_of(intArray)
  ref2 = ref_of(ref1)
  ref3 = ref(ref2)

  ptr2d => intXY(ref3)


  do i = 1, 300
    do j = 1, 300
      ref1 = ref_of(ref2)
    end do
  end do


  ref1 = ref_of(cplx)

  cplx_ptr => complex128(ref1)
  cplx_ptr = (1.0, -3)


  ref2 = ref_of(42)
  ref1 = ref_of(ref2)
  print *, int32(ref2)
  print *, int32(ref(ref1))

  if (is_ref(ref1)) then
    ref3 = ref(ref1)
    if (is_int32(ref3)) &
      print *, int32(ref3)
  end if

  ref3 = clone(ref1)
  print *, int32(ref(ref3))
  call free( ref3 )

  ref1 = ref_of(intArray)
  ptr2d => intXY(ref1)
  ptr2d = 42
  cpointer = cptr(ref1)

  ref2 = ref_of(4.23)
  ref1 = clone(ref2)

  call free(ref1)

  ref1 = ref_of(dong)

  ref2 = clone(ref1)

  call free(ref2)

  allocate( ptr2d(4,4) )
  ref1 = ref_of(ptr2d)

  ptr2d => null()
  ptr2d => intXY(ref1)

  ptr2d = 21

  ref2 = ref_from_CallBack( sub_a )
  print *, rank(ref2)
  print *, shape(ref2)
  ref2 = clone(ref2)

  sc => CallBack_from_ref( ref2 )
  call sc()

  ref2 = ref_from_CalcFunc( func_a )

  f => CalcFunc_from_ref( ref2 )
  print *, f( 2.43 )

  print *, shape(ref1)
  ref2 = clone(ref1)
  print *, shape(ref2)
  print *, intXY(ref2)

  call free(ref1)
  call free(ref2)
  call delete( ref1 )

end

