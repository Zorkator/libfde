
#include "adt/ppUtil.xpp"

module test_basedata
  use adt_basetypes
  use adt_string
  use adt_ref
  use adt_item
  use adt_list
  use adt_hashmap
  use adt_visitor
  use adt_ostream
  use iso_c_binding
  implicit none

  !---------------------------
  ! declare basetypes ...
  !---------------------------
  logical*1         :: v_bool1      = .true.!{{{
  logical*2         :: v_bool2      = .true.
  logical*4         :: v_bool4      = .true.
  logical*8         :: v_bool8      = .true.
  integer*1         :: v_int1       = 1
  integer*2         :: v_int2       = 2
  integer*4         :: v_int4       = 3
  integer*8         :: v_int8       = 4
  real*4            :: v_real4      = 0.1
  real*8            :: v_real8      = 0.2
  !real*16           :: v_real16     = 0.3
  complex*8         :: v_complex8   = (1.0,-2.0)
  complex*16        :: v_complex16  = (2.0,-2.0)
  !complex*32        :: v_complex32  = (3.0,-2.0)
  type(c_ptr)       :: v_c_void_ptr = C_NULL_PTR
  character(len=10) :: v_char10     = "abcde"
  type(String_t)    :: v_string
  type(Ref_t)       :: v_ref
  type(Item_t)      :: v_item
  type(List_t)      :: v_list
  type(HashMap_t)   :: v_hashmap

  logical*1,         dimension(:), allocatable :: v_bool1_1d
  logical*2,         dimension(:), allocatable :: v_bool2_1d
  logical*4,         dimension(:), allocatable :: v_bool4_1d
  logical*8,         dimension(:), allocatable :: v_bool8_1d
  integer*1,         dimension(:), allocatable :: v_int1_1d
  integer*2,         dimension(:), allocatable :: v_int2_1d
  integer*4,         dimension(:), allocatable :: v_int4_1d
  integer*8,         dimension(:), allocatable :: v_int8_1d
  real*4,            dimension(:), allocatable :: v_real4_1d
  real*8,            dimension(:), allocatable :: v_real8_1d
  !real*16,           dimension(:), allocatable :: v_real16_1d
  complex*8,         dimension(:), allocatable :: v_complex8_1d
  complex*16,        dimension(:), allocatable :: v_complex16_1d
  !complex*32,        dimension(:), allocatable :: v_complex32_1d
  type(c_ptr),       dimension(:), allocatable :: v_c_void_ptr_1d
  character(len=10), dimension(:), allocatable :: v_char10_1d
  type(String_t),    dimension(:), allocatable :: v_string_1d
  type(Ref_t),       dimension(:), allocatable :: v_ref_1d
  type(Item_t),      dimension(:), allocatable :: v_item_1d
  type(List_t),      dimension(:), allocatable :: v_list_1d
  type(HashMap_t),   dimension(:), allocatable :: v_hashmap_1d

  logical*1,         dimension(:,:), allocatable :: v_bool1_2d
  logical*2,         dimension(:,:), allocatable :: v_bool2_2d
  logical*4,         dimension(:,:), allocatable :: v_bool4_2d
  logical*8,         dimension(:,:), allocatable :: v_bool8_2d
  integer*1,         dimension(:,:), allocatable :: v_int1_2d
  integer*2,         dimension(:,:), allocatable :: v_int2_2d
  integer*4,         dimension(:,:), allocatable :: v_int4_2d
  integer*8,         dimension(:,:), allocatable :: v_int8_2d
  real*4,            dimension(:,:), allocatable :: v_real4_2d
  real*8,            dimension(:,:), allocatable :: v_real8_2d
  !real*16,           dimension(:,:), allocatable :: v_real16_2d
  complex*8,         dimension(:,:), allocatable :: v_complex8_2d
  complex*16,        dimension(:,:), allocatable :: v_complex16_2d
  !complex*32,        dimension(:,:), allocatable :: v_complex32_2d
  type(c_ptr),       dimension(:,:), allocatable :: v_c_void_ptr_2d
  character(len=10), dimension(:,:), allocatable :: v_char10_2d
  type(String_t),    dimension(:,:), allocatable :: v_string_2d
  type(Ref_t),       dimension(:,:), allocatable :: v_ref_2d
  type(Item_t),      dimension(:,:), allocatable :: v_item_2d
  type(List_t),      dimension(:,:), allocatable :: v_list_2d
  type(HashMap_t),   dimension(:,:), allocatable :: v_hashmap_2d

  !---------------------------------------
  ! declare pointers to basetypes ...
  !---------------------------------------
  logical*1       , pointer :: p_bool1
  logical*2       , pointer :: p_bool2
  logical*4       , pointer :: p_bool4
  logical*8       , pointer :: p_bool8
  integer*1       , pointer :: p_int1 
  integer*2       , pointer :: p_int2 
  integer*4       , pointer :: p_int4 
  integer*8       , pointer :: p_int8 
  real*4          , pointer :: p_real4
  real*8          , pointer :: p_real8
  !real*16        , pointer  :: p_real16
  complex*8       , pointer :: p_complex8
  complex*16      , pointer :: p_complex16
  !complex*32     , pointer  :: p_complex32
  type(c_ptr)     , pointer :: p_c_void_ptr
  character(len=:), pointer :: p_char10
  type(String_t)  , pointer :: p_string
  type(Ref_t)     , pointer :: p_ref
  type(Item_t)    , pointer :: p_item
  type(List_t)    , pointer :: p_list
  type(HashMap_t) , pointer :: p_hashmap

  logical*1,        dimension(:), pointer :: p_bool1_1d
  logical*2,        dimension(:), pointer :: p_bool2_1d
  logical*4,        dimension(:), pointer :: p_bool4_1d
  logical*8,        dimension(:), pointer :: p_bool8_1d
  integer*1,        dimension(:), pointer :: p_int1_1d
  integer*2,        dimension(:), pointer :: p_int2_1d
  integer*4,        dimension(:), pointer :: p_int4_1d
  integer*8,        dimension(:), pointer :: p_int8_1d
  real*4,           dimension(:), pointer :: p_real4_1d
  real*8,           dimension(:), pointer :: p_real8_1d
  !real*16,          dimension(:), pointer :: p_real16_1d
  complex*8,        dimension(:), pointer :: p_complex8_1d
  complex*16,       dimension(:), pointer :: p_complex16_1d
  !complex*32,       dimension(:), pointer :: p_complex32_1d
  type(c_ptr),      dimension(:), pointer :: p_c_void_ptr_1d
  character(len=:), dimension(:), pointer :: p_char10_1d
  type(String_t),   dimension(:), pointer :: p_string_1d
  type(Ref_t),      dimension(:), pointer :: p_ref_1d
  type(Item_t),     dimension(:), pointer :: p_item_1d
  type(List_t),     dimension(:), pointer :: p_list_1d
  type(HashMap_t),  dimension(:), pointer :: p_hashmap_1d

  logical*1,        dimension(:,:), pointer :: p_bool1_2d
  logical*2,        dimension(:,:), pointer :: p_bool2_2d
  logical*4,        dimension(:,:), pointer :: p_bool4_2d
  logical*8,        dimension(:,:), pointer :: p_bool8_2d
  integer*1,        dimension(:,:), pointer :: p_int1_2d
  integer*2,        dimension(:,:), pointer :: p_int2_2d
  integer*4,        dimension(:,:), pointer :: p_int4_2d
  integer*8,        dimension(:,:), pointer :: p_int8_2d
  real*4,           dimension(:,:), pointer :: p_real4_2d
  real*8,           dimension(:,:), pointer :: p_real8_2d
  !real*16,          dimension(:,:), pointer :: p_real16_2d
  complex*8,        dimension(:,:), pointer :: p_complex8_2d
  complex*16,       dimension(:,:), pointer :: p_complex16_2d
  !complex*32,       dimension(:,:), pointer :: p_complex32_2d
  type(c_ptr),      dimension(:,:), pointer :: p_c_void_ptr_2d
  character(len=:), dimension(:,:), pointer :: p_char10_2d
  type(String_t),   dimension(:,:), pointer :: p_string_2d
  type(Ref_t),      dimension(:,:), pointer :: p_ref_2d
  type(Item_t),     dimension(:,:), pointer :: p_item_2d
  type(List_t),     dimension(:,:), pointer :: p_list_2d
  type(HashMap_t),  dimension(:,:), pointer :: p_hashmap_2d!}}}

  type(StreamVisitor_t) :: streamer
  type(ostream_t)       :: fout

  contains

  subroutine init_basedata()!{{{
    implicit none
    integer :: i, j

    call initialize( v_list )
    call initialize( v_hashmap )

    allocate( v_bool1_1d(5) );        v_bool1_1d      = .true.
    allocate( v_bool2_1d(5) );        v_bool2_1d      = .true.
    allocate( v_bool4_1d(5) );        v_bool4_1d      = .true.
    allocate( v_bool8_1d(5) );        v_bool8_1d      = .true.
    allocate( v_int1_1d(5) );         v_int1_1d       = 1
    allocate( v_int2_1d(5) );         v_int2_1d       = 2
    allocate( v_int4_1d(5) );         v_int4_1d       = 3
    allocate( v_int8_1d(5) );         v_int8_1d       = 4
    allocate( v_real4_1d(5) );        v_real4_1d      = 0.1
    allocate( v_real8_1d(5) );        v_real8_1d      = 0.2
    !allocate( v_real16_1d(5) );       v_real16_1d     = 0.3        
    allocate( v_complex8_1d(5) );     v_complex8_1d   = (1.0,-2.0)   
    allocate( v_complex16_1d(5) );    v_complex16_1d  = (2.0,-2.0)    
    !allocate( v_complex32_1d(5) );    v_complex32_1d  = (3.0,-2.0)    
    allocate( v_c_void_ptr_1d(5) );
    allocate( v_char10_1d(5) );       v_char10_1d     = "abcde"
    allocate( v_string_1d(5) );
    allocate( v_ref_1d(5) );
    allocate( v_item_1d(5) );
    allocate( v_list_1d(5) );
    allocate( v_hashmap_1d(5) );

    do i = 1, 5
      call initialize( v_list_1d(i) )
      call initialize( v_hashmap_1d(i) )
    end do

    allocate( v_bool1_2d(2,2) );      v_bool1_2d      = .true.
    allocate( v_bool2_2d(2,2) );      v_bool2_2d      = .true.
    allocate( v_bool4_2d(2,2) );      v_bool4_2d      = .true.
    allocate( v_bool8_2d(2,2) );      v_bool8_2d      = .true.
    allocate( v_int1_2d(2,2) );       v_int1_2d       = 1
    allocate( v_int2_2d(2,2) );       v_int2_2d       = 2
    allocate( v_int4_2d(2,2) );       v_int4_2d       = 3
    allocate( v_int8_2d(2,2) );       v_int8_2d       = 4
    allocate( v_real4_2d(2,2) );      v_real4_2d      = 0.1
    allocate( v_real8_2d(2,2) );      v_real8_2d      = 0.2
    !allocate( v_real16_2d(2,2) );     v_real16_2d     = 0.3             
    allocate( v_complex8_2d(2,2) );   v_complex8_2d   = (1.0,-2.0)        
    allocate( v_complex16_2d(2,2) );  v_complex16_2d  = (2.0,-2.0)         
    !allocate( v_complex32_2d(2,2) );  v_complex32_2d  = (3.0,-2.0)         
    allocate( v_c_void_ptr_2d(2,2) ); v_c_void_ptr_2d = C_NULL_PTR
    allocate( v_char10_2d(2,2) );     v_char10_2d     = "abcde"
    allocate( v_string_2d(2,2) );
    allocate( v_ref_2d(2,2) );
    allocate( v_item_2d(2,2) );
    allocate( v_list_2d(2,2) );
    allocate( v_hashmap_2d(2,2) );

    do i = 1, 2
      do j = 1, 2
        call initialize( v_list_2d(i,j) )
        call initialize( v_hashmap_2d(i,j) )
      end do
    end do
    
    streamer = StreamVisitor( 6 )
    fout     = ostream( 6 )
  end subroutine!}}}


  subroutine cleanup_basedata()!{{{
    implicit none
    integer :: i, j

    call delete( v_list )
    call delete( v_hashmap )

    do i = 1, 5
      call delete( v_list_1d(i) )
      call delete( v_hashmap_1d(i) )
    end do

    deallocate( v_hashmap_1d )
    deallocate( v_list_1d )
    deallocate( v_item_1d )
    deallocate( v_ref_1d )
    deallocate( v_string_1d )
    deallocate( v_char10_1d )
    deallocate( v_c_void_ptr_1d )
    !deallocate( v_complex32_1d )
    deallocate( v_complex16_1d )
    deallocate( v_complex8_1d )
    !deallocate( v_real16_1d )
    deallocate( v_real8_1d )
    deallocate( v_real4_1d )
    deallocate( v_int8_1d )
    deallocate( v_int4_1d )
    deallocate( v_int2_1d )
    deallocate( v_int1_1d )
    deallocate( v_bool8_1d )
    deallocate( v_bool4_1d )
    deallocate( v_bool2_1d )
    deallocate( v_bool1_1d )

    do i = 1, 2
      do j = 1, 2
        call delete( v_list_2d(i,j) )
        call delete( v_hashmap_2d(i,j) )
      end do
    end do

    deallocate( v_hashmap_2d )
    deallocate( v_list_2d )
    deallocate( v_item_2d )
    deallocate( v_ref_2d )
    deallocate( v_string_2d )
    deallocate( v_char10_2d )
    deallocate( v_c_void_ptr_2d )
    !deallocate( v_complex32_2d )
    deallocate( v_complex16_2d )
    deallocate( v_complex8_2d )
    !deallocate( v_real16_2d )
    deallocate( v_real8_2d )
    deallocate( v_real4_2d )
    deallocate( v_int8_2d )
    deallocate( v_int4_2d )
    deallocate( v_int2_2d )
    deallocate( v_int1_2d )
    deallocate( v_bool8_2d )
    deallocate( v_bool4_2d )
    deallocate( v_bool2_2d )
    deallocate( v_bool1_2d )
  end subroutine!}}}

end module


!module adt_alloc!{{{
!
!  interface adt_allocate
!    module procedure hashmap_allocate
!  end interface
!
!  interface adt_alloc_ref
!    module procedure hashmap_allocate_ref__, real8_1d_allocate_ref__
!  end interface
!
!  contains
!
!  subroutine hashmap_allocate_ref__( ptr, reference, bind )
!    use adt_hashmap
!    use adt_ref, only: Ref_t
!    implicit none
!    type(HashMap_t), pointer,       intent(inout) :: ptr
!    type(Ref_t), optional,          intent(inout) :: reference
!    logical, optional                             :: bind
!
!    allocate( ptr )
!    if (present(reference)) then
!      reference = ref_of( ptr, bind )
!    end if
!  end subroutine
!
!  
!  subroutine real8_1d_allocate_ref__( ptr, d1, reference, bind )
!    use adt_ref, only: Ref_t
!    use adt_basetypes
!    implicit none
!    real*8, dimension(:), pointer,  intent(inout) :: ptr
!    integer                                       :: d1
!    type(Ref_t), optional,          intent(inout) :: reference
!    logical, optional                             :: bind
!
!    allocate( ptr(d1) )
!    if (present(reference)) then
!      reference = ref_of( ptr, bind )
!    end if
!  end subroutine
!  
!
!  function hashmap_allocate_ref( ptr ) result(res)
!    use adt_hashmap
!    use adt_ref
!    implicit none
!    type(HashMap_t),  pointer,       intent(inout) :: ptr
!    type(Ref_t)                                    :: res
!    allocate( ptr )
!    call set_attribute( res, attribute_volatile )
!    res = ref_of( ptr, bind = .true. )
!  end function
!
!  subroutine hashmap_allocate( ptr, map, id )
!    use adt_hashmap
!    use adt_item
!    implicit none
!    type(HashMap_t),  pointer,       intent(inout) :: ptr
!    type(HashMap_t),  optional,      intent(inout) :: map
!    character(len=*), optional, target, intent(in) :: id
!    character(len=10),          target             :: buffer
!    character(len=:), pointer                      :: id_ptr
!
!    allocate( ptr )
!    if (present( map )) then
!      if (present( id )) then
!        id_ptr => id
!      else
!        write(buffer,'(A2,Z8.8)') '0x', loc(ptr)
!        id_ptr => buffer
!      end if
!      call set( map, id_ptr, Item_of( ref_of(ptr) ) )
!    end if
!  end subroutine
!
!end module!}}}


subroutine do_assert( expr_bool, expr_str )!{{{
  logical,          intent(in) :: expr_bool
  character(len=*), intent(in) :: expr_str
  if (.not. expr_bool) &
    print *, expr_str // ': ', expr_bool
end subroutine

# define _assert( expr ) \
    call do_assert( expr, _str(expr) )

# define _assert_not( expr ) \
    call do_assert( .not. expr, _str(expr) )
!}}}

subroutine test_string()!{{{
  use test_basedata
  implicit none

end subroutine!}}}


subroutine test_ref()!{{{
  use test_basedata
  implicit none

  type(Ref_t) :: r1, r2

  r1 = ref_of( v_bool1 )
  call accept( r1, streamer%super )
  r1 = ref_of( v_bool1_1d )
  call accept( r1, streamer%super )

  r1 = ref_of( v_bool1, bind = .false. )
  _assert( is_valid( r1 ) )
  _assert_not( is_valid( r2 ) )
  r2 = ref_of( v_bool1 )
  _assert( r1 == r2 )
  r2 = clone( r1 )
  _assert( r1 /= r2 )
  r1 = r2
  _assert( r1 == r2 )
  r1 = ref_of( v_int4 )
  r2 = ref_of( r1 )
  _assert( int4( resolve( r2 ) ) == 3 )

  r1 = ref_of( v_bool1 )
  print *, bool1(r1)
  r2 = clone( r1 )
  print *, bool1(r2)
  r2 = clone( ref_of( v_bool1 ) )
  print *, bool1(r2)
  _assert( r1 /= r2 )
  print *, bool1(r1), bool1(r2)

# define _ref_deref_( typeId )        \
    r1 = ref_of( _paste(v_,typeId) ) ;\
    _paste(p_,typeId) => typeId(r1)  ;\
    r2 = clone( r1 )                 ;\
    r2 = clone( ref_of( _paste(v_,typeId) ) ) ;\
    print *, _str(typeId), loc(_paste(p_,typeId)) == loc(_paste(v_,typeId)), .not. (r2 == r1); \
    _paste(p_,typeId) => typeId(r2)

  _ref_deref_(bool1)
  _ref_deref_(bool2)
  _ref_deref_(bool4)
  _ref_deref_(bool8)
  _ref_deref_(int1)
  _ref_deref_(int2)
  _ref_deref_(int4)
  _ref_deref_(int8)
  _ref_deref_(real4)
  _ref_deref_(real8)
  _ref_deref_(complex8)
  _ref_deref_(complex16)
#ifdef __GNUC__
  print *, "skipping 'c_void_ptr' since gfortran doesn't support it"
#else
  _ref_deref_(c_void_ptr)
#endif
  _ref_deref_(char10)
  _ref_deref_(string)
  _ref_deref_(ref)
  _ref_deref_(item)
  _ref_deref_(list)
  _ref_deref_(hashmap)

  call delete( r1 )
  call delete( r2 )

end subroutine!}}}


subroutine test_item()!{{{
  use test_basedata
  implicit none
  type(Item_t) :: item_array(5)

  !item_array(1:) = v_item_1d(1:) !< shallow copy! fortran can't do proper array assignment with derived types!!!
end subroutine!}}}


subroutine test_list()!{{{
  use test_basedata
  implicit none
  !type(List_t) :: list_array(5)
  type(ListIndex_t)         :: idx
  type(TypeInfo_t), pointer :: ti

  !list_array(1:) = v_list_1d(1:) !< shallow copy! fortran can't do proper array assignment with derived types!!!
# define _list_append( typeId ) \
    call append( v_list, new_ListNode_of( _paste(v_,typeId) ) )
  
# define _list_append_ref( typeId ) \
    call append( v_list, new_ListNode_of( ref_of(_paste(v_,typeId)) ) )
  
  _list_append(bool1)
  _list_append(bool2)
  _list_append(bool4)
  _list_append(bool8)
  _list_append(int1)
  _list_append(int2)
  _list_append(int4)
  _list_append(int8)
  _list_append(real4)
  _list_append(real8)
  _list_append(complex8)
  _list_append(complex16)
  _list_append(c_void_ptr)
  _list_append(char10)
  _list_append(string)
  _list_append(ref)
  _list_append(item)
  !_list_append(list)
  !_list_append(hashmap)
  _list_append_ref(bool1_1d)
  _list_append_ref(bool2_1d)
  _list_append_ref(bool4_1d)
  _list_append_ref(bool8_1d)
  _list_append_ref(int1_1d)
  _list_append_ref(int2_1d)
  _list_append_ref(int4_1d)
  _list_append_ref(int8_1d)
  _list_append_ref(real4_1d)
  _list_append_ref(real8_1d)
  _list_append_ref(complex8_1d)
  _list_append_ref(complex16_1d)
  _list_append_ref(c_void_ptr_1d)
  _list_append_ref(char10_1d)
  _list_append_ref(string_1d)
  !_list_append_ref(ref_1d)
  !_list_append_ref(item_1d)
  _list_append_ref(bool1_2d)
  _list_append_ref(bool2_2d)
  _list_append_ref(bool4_2d)
  _list_append_ref(bool8_2d)
  _list_append_ref(int1_2d)
  _list_append_ref(int2_2d)
  _list_append_ref(int4_2d)
  _list_append_ref(int8_2d)
  _list_append_ref(real4_2d)
  _list_append_ref(real8_2d)
  _list_append_ref(complex8_2d)
  _list_append_ref(complex16_2d)
  _list_append_ref(c_void_ptr_2d)
  _list_append_ref(char10_2d)
  !_list_append_ref(string_2d)
  !_list_append_ref(ref_2d)
  !_list_append_ref(item_2d)

  idx = index( v_list )
  do while (is_valid(idx))
    ti => dynamic_type( idx )
    print *, trim(ti%baseType)
    call next( idx )
  end do

  call accept( get( v_hashmap, 'initial matrix-clone' ), streamer%super )
  
  
  v_ref = ref_of( v_list )
  call accept( v_ref, streamer%super )

  call delete( v_list )

end subroutine!}}}


subroutine test_hashmap()!{{{
  use test_basedata
  implicit none

  type(Ref_t),      pointer :: ref_ptr
  type(Ref_t)               :: ref_1
  type(HashMapIndex_t)      :: idx
  type(Item_t),     pointer :: val
  type(TypeInfo_t), pointer :: ti

# define _map_get( typeId ) \
    _paste(p_,typeId) => typeId( get( v_hashmap, _str(typeId) ) ) ;\
    _paste(p_,typeId) = _paste(v_,typeId)

# define _map_ref( typeId ) \
    ref_ptr => ref( get( v_hashmap, _str(typeId) ) ) ;\
    ref_ptr = ref_of( _paste(v_,typeId) )

  ! fist, we add an array clone to test hooking weak references.
  ! This is for checking static_type() on weak Ref_t's if typeInfo has NOT been initialized yet!
  call set( v_hashmap, 'initial matrix-clone', Item_of( clone( ref_of(v_int4_2d) ) ) )

  _map_get(bool1)
  _map_get(bool2)
  _map_get(bool4)
  _map_get(bool8)
  _map_get(int1)
  _map_get(int2)
  _map_get(int4)
  _map_get(int8)
  _map_get(real4)
  _map_get(real8)
  _map_get(complex8)
  _map_get(complex16)
  _map_get(c_void_ptr)
  _map_get(string)
  _map_get(ref)
  _map_ref(char10)
  _map_ref(item)
  _map_ref(list)
  _map_ref(hashmap)

  call set( v_hashmap, 'submap', Item_of( clone( ref_ptr ) ) )
  p_hashmap => hashmap( ref( get( v_hashmap, 'submap' ) ) )
  call set( p_hashmap, 'subsubmap', Item_of( clone( ref_ptr ) ) )

  idx = index( p_hashmap )
  do while (is_valid(idx))
    val => value(idx)
    if (is_ref(val)) then
      ref_ptr => ref( val )
      ti      => dynamic_type( ref_ptr )
    else
      ti => dynamic_type( val )
    end if
    print *, str(key(idx)), ' => ', trim(ti%baseType)
    call next( idx )
  end do
  
  call delete( ref_1 )

end subroutine!}}}


# define fileScope() \
    trim(adjustl(file_basename( __FILE__ )))


subroutine test_hashmap_nesting()!{{{
  use test_basedata
  !use adt_alloc
  use adt_scope
  implicit none

  type(HashMap_t), pointer :: scope => null()
  type(Ref_t)              :: ref1
  integer                  :: i
  character(len=10)        :: buff
  real*8, dimension(:), pointer :: r_array

  ref1 = ref_of( newScope(), bind = .true. )

  r_array => null()
  print *, dynamic_cast( r_array, ref1 ) !< should fail
  print *, dynamic_cast( scope, ref1 )   !< should succeed

  scope => getScope( scope, fileScope() )
  do i = 1,3
    write(buff,'(A7,I1)'), 'submap_', i
    call set( scope, trim(buff), Item_of( ref_of( newScope(), bind = .true. ) ) )
  end do

  allocate( r_array(10) )
  r_array = [1,2,3,4,5,6,7,8,9,0]
  scope => getScope( scope, 'gcsm' )
  call set( scope, 'text', Item_of( ref_of(r_array, bind=.true.) ) )
  call set( scope, 'text', Item_of( ref_of(buff) ) )
  call set( getScope( scope, 'signal'), 'value', Item_of( ref_of(i) ) )

  allocate( r_array(10) )
  scope => getScope( hashmap(ref1), 'gcsm' )
  call set( scope, 'counter', Item_of( ref_of(i) ) )
  call set( getScope( scope, 'signal'), 'value', Item_of( ref_of(r_array, bind = .true.) ) )
  !call print_scope( hashmap(ref1), 0 )

  scope => getScope( getScope( getScope( hashmap(ref1), fileScope() ), 'gcsm' ), 'signal' )
  scope => getScope( hashmap(ref1), fileScope(), 'gcsm', 'signal' )
  
  if (dynamic_cast( r_array, ref(get(scope, 'value')) )) &
    print *, r_array

  call stream( scope, fout )

  call accept( scope, streamer%super )
  call accept( ref1, streamer%super )

  !call print_scope( hashmap(ref1), 1 )
  call delete( ref1 )
end subroutine!}}}


subroutine test_hashmap_cloning()!{{{
  use test_basedata
  use adt_scope

  type(HashMap_t)               :: map
  type(HashMap_t),      pointer :: mapPtr
  type(Ref_t)                   :: r
  real*8, dimension(:), pointer :: r_array, r_ptr

  r = ref_of( newScope(), bind = .true. )
  allocate( r_array(10) ); r_array = 1
  call set( HashMap(r), "array", Item_of( ref_of( r_array, bind = .true. ) ) )
  ! python: r_array = [1,1,1,1,1,1,1,1,1,1]
  ! python: r = dict( array = r_array )
  
  map = HashMap(r) !< python: map = dict( **r )
  ! python: both dicts refer to r_array !!!
  
  r_ptr => real8_1d( ref( get( map, 'array' ) ) )
  ! python: r_ptr = map['array']
  ! CAUTION: r_ptr and r_array refer to same array!

  call delete( map )
  call delete( r )
  ! CAUTION: r_array invalid here!
  
  call initialize( map )
  allocate( r_array(10) ); r_array = 1
  call set( map, "array", Item_of( ref_of( r_array, bind=.true. ) ) )

  r = clone( ref_of( map ) )
  mapPtr => HashMap(r)

  r_array => real8_1d( ref( get( map, 'array' ) ) )
  r_ptr   => real8_1d( ref( get( map, 'array' ) ) )

  r_array = 2
  print *, r_array == r_ptr !< should print T, since both refer to same array!

  call set( mapPtr, "array", Item_of( clone( ref_of(r_ptr) ) ) )

  r_array => real8_1d( ref( get( map, 'array' ) ) )
  r_ptr   => real8_1d( ref( get( mapPtr, 'array' ) ) )

  print *, r_array == r_ptr !< should print T, since arrays are equal!
  r_ptr = 0
  print *, r_array /= r_ptr !< should print T, since arrays are NOT identical!

  call delete( map )
  call delete( r )

end subroutine!}}}


subroutine test_file_string()!{{{
  use adt_string
  type(String_t) :: s

  s = "c:\path/test/blub.f"
  s = file_basename(s)
  s = fileScope()

  print *, fileScope()
  print *, file_basename("\.testinger")
  print *, file_basename("/.testinger\bla")
  print *, file_basename(".testinger")
  print *, file_basename("testinger.f90")
  print *, file_basename("path\testinger.f90")
  print *, file_basename("c:\path/testinger.f90")
  print *, file_basename("path/testinger")
  print *, file_basename("testinger")

  call delete(s)
end subroutine!}}}


subroutine test_visitor()
  use test_basedata

# define _visit_(typeId) \
    call accept( _paste(v_,typeId), streamer%super )

  _visit_(bool1)
  _visit_(bool2)
  _visit_(bool4)
  _visit_(bool8)
  _visit_(int1)
  _visit_(int2)
  _visit_(int4)
  _visit_(int8)
  _visit_(real4)
  _visit_(real8)
  _visit_(complex8)
  _visit_(complex16)
  _visit_(c_void_ptr)
  _visit_(string)
  _visit_(ref)
  _visit_(char10)
  _visit_(item)
  _visit_(list)
  _visit_(hashmap)

  v_ref = ref_of( v_complex16 )
  call accept( v_ref, streamer%super )
  v_item = Item_of( ref_of( v_real8 ) )
  call accept( v_item, streamer%super )
  
end subroutine


module visitor_testmod!{{{
  use adt_visitor
  implicit none

  type node_t
    type(node_t), pointer :: next => null()
    integer               :: value = -1
  end type

  type(node_t), target  :: list

  type CountVisitor_t
    type(Visitor_t)              :: super
    procedure(), nopass, pointer :: visit
    integer                      :: sum = 0
  end type

  contains

  subroutine CountVisitor_step( v, n )
    type(CountVisitor_t) :: v
    type(node_t)         :: n
    v%sum = v%sum + 1
    call v%visit( v, n )
  end subroutine

  function CountVisitor( func ) result(res)
    external             :: func
    type(CountVisitor_t) :: res
    res%super%visit => CountVisitor_step
    res%visit       => func
  end function
  

  subroutine init_list()
    type(node_t), pointer :: ptr
    integer               :: i
    
    ptr => list
    do i = 1, 5
      allocate(ptr%next)
      ptr%value = i
      ptr => ptr%next
    end do
  end subroutine

  recursive &
  subroutine accept( n, v )
    type(node_t)    :: n
    type(visitor_t) :: v

    call v%visit( v, n )
    if (associated(n%next)) &
      call accept( n%next, v )
  end subroutine

  subroutine test()
    type(CountVisitor_t) :: v

    call init_list()
    call accept( list, Visitor(blub) )
    v = CountVisitor(blub)
    call accept( list, v%super )
    print *, v%sum

    contains

    subroutine blub( v, n )
      type(visitor_t) :: v
      type(node_t)    :: n
      print *, 'blub', n%value 
    end subroutine
  end subroutine

end module!}}}


program test_adt
  use test_basedata
  use adt_convert

  integer,   target :: dings = 324
  character(len=18) :: res

  call width( fout, 5 )
  call write( fout, "testinger" )
  call width( fout, 15 )
  call write( fout, "test" )
  call write( fout, "testing" )
  call write( fout, "testinger" )
  call width( fout, 0 )
  call write( fout, "testinger" )
  call write( fout, "testinger" )
  call newline( fout, 0 )
  call write( fout, "testinger" )
  call newline( fout, 2 )
  call write( fout, "testinger" )
  call newline( fout, 1 )
  call write( fout, "testinger" )
  call newline( fout, 5 )
  call write( fout, "testinger" )
  call width( fout, 32 )
  call write( fout, "testinger" )

  print *, hex(dings)
  print *, address_str( c_loc(dings) )

  call init_basedata()

  call stream( v_bool1_1d, fout )

  call test_visitor()
  call test_hashmap()
  call test_string()
  call test_ref()
  call test_item()
  call test_list()
  call test_hashmap_nesting()
  call test_hashmap_cloning()
  call test_file_string()

  call cleanup_basedata()
  call hashmap_clear_cache()

end
