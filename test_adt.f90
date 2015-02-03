
#include "adt/ppUtil.xpp"

module test_basedata
  use adt_basetypes
  use adt_string
  use adt_ref
  use adt_item
  use adt_list
  use adt_hashmap
  use iso_c_binding
  implicit none

  !---------------------------
  ! declare basetypes ...
  !---------------------------
  logical*1         :: v_bool1      = .true.
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
  type(HashMap_t),  dimension(:,:), pointer :: p_hashmap_2d

  contains

  subroutine init_basedata()
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
  end subroutine


  subroutine cleanup_basedata()
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
  end subroutine

end module


subroutine do_assert( expr_bool, expr_str )
  logical,          intent(in) :: expr_bool
  character(len=*), intent(in) :: expr_str
  if (.not. expr_bool) &
    print *, expr_str // ': ', expr_bool
end subroutine

# define _assert( expr ) \
    call do_assert( expr, _str(expr) )

# define _assert_not( expr ) \
    call do_assert( .not. expr, _str(expr) )


subroutine test_string()
  use test_basedata
  implicit none

end subroutine


subroutine test_ref()
  use test_basedata
  implicit none

  type(Ref_t) :: r1, r2

  r1 = ref_of( v_bool1 )
  _assert( is_valid( r1 ) )
  _assert_not( is_valid( r2 ) )
  r2 = ref_of( v_bool1 )
  _assert( r1 == r2 )
  r2 = clone( r1 )
  _assert( r1 /= r2 )
  r1 = r2
  _assert( r1 == r2 )

# define _ref_deref_( typeId )        \
    r1 = ref_of( _paste(v_,typeId) ) ;\
    _paste(p_,typeId) => typeId(r1)  ;\
    r2 = clone( r1 ) ;\
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
  _ref_deref_(c_void_ptr)
  _ref_deref_(char10)
  _ref_deref_(string)
  _ref_deref_(ref)
  _ref_deref_(item)
  _ref_deref_(list)
  _ref_deref_(hashmap)

  call delete( r1 )
  call delete( r2 )

end subroutine


subroutine test_item()
  use test_basedata
  implicit none

end subroutine


subroutine test_list()
  use test_basedata
  implicit none

end subroutine


subroutine test_hashmap()
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

end subroutine




program test_adt
  use test_basedata

  call init_basedata()
  call test_string()
  call test_ref()
  call test_item()
  call test_list()
  call test_hashmap()

  call cleanup_basedata()
  call hashmap_clear_cache()

end
