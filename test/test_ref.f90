
program testinger
  use adt_ref
  use type_references
  use adt_basetypes
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
  procedure(func),         pointer :: f  => null()
  procedure(Callback_itf), pointer :: sc => null()
  type(Ding)                       :: dong
  integer*4                        :: i, j
  type(TypeInfo_t),        pointer :: ti

  type(c_ptr)         :: cpointer
  type(Ref_t)         :: ref1, ref2, ref3, ref4
  complex*32          :: cplx
  complex*32, pointer :: cplx_ptr

  i = 0; j = 1

  ref2 = clone(ref_of(j))
  print *, int4(ref2)

  ref1 = ref_of(i)
  call delete(ref1)

  allocate( ptr2d(3,4) )
  ref1 = ref_of( ptr2d )
  call bind( ref1, .true. )
  ptr2d => null()
  ref2 = ref1

  ref1 = ref_of(i)
  ref2 = ref_of(ref1)
  call delete(ref1)

  ref1 = ref_of(i)
  ref2 = ref_of(j)

  ref3 = ref_of(j)
  ref2 = clone(ref3)
  print *, int4(ref2)

  ref2 = clone(ref_of(j))
  print *, int4(ref2)

  ref2 = clone(ref1)
  call delete(ref2)

  ref2 = clone(ref1)
  ref2 = ref_of(j)
  call delete(ref2)

  ref2 = ref_of(ref1)
  ref3 = ref_of(ref2)
  ref4 = clone(ref3)
  call delete(ref4)


  intArray = 34

  ref1 = ref_of(intArray)
  ref2 = ref_of(ref1)
  ref3 = ref(ref2)

  ptr2d => int4_2d(ref3)


  do i = 1, 300
    do j = 1, 300
      ref1 = ref_of(ref2)
    end do
  end do


  ref1 = ref_of(cplx)

  cplx_ptr => complex32(ref1)
  cplx_ptr = (1.0, -3)


  ref2 = ref_of(42)
  ref1 = ref_of(ref2)
  print *, int4(ref2)
  print *, int4(ref(ref1))

  if (is_ref(ref1)) then
    ref3 = ref(ref1)
    if (is_int4(ref3)) &
      print *, int4(ref3)
  end if

  ref3 = clone(ref1)
  print *, int4(ref(ref3))
  call delete( ref3 )

  ref1 = ref_of(intArray)
  ptr2d => int4_2d(ref1)
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
  ti => content_type(ref1)
  call bind( ref1, .false. )
  call bind( ref1, .true. )

  ptr2d => null()
  ptr2d => int4_2d(ref1)
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
  print *, int4_2d(ref2)

  call delete(ref1)
  call delete(ref2)
  call delete(ref3)

end

