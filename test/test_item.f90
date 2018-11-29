
#include "fde/item.fpp"

program testinger
  use fde_item
  use fde_ref
  use fde_typeinfo
  use fde_basetypes
  use fde_string
  implicit none

  type(Item_t)     :: v1, v2
  type(TypeInfo_t) :: ti
  integer*4        :: intvar, i, j
  type(String_t)   :: ds
  type(Ref_t)      :: gr, gr2
  real*4           :: realVal
  logical          :: logic

  print *, "Item: ",       storage_size(v1)/8
  print *, "String: ", storage_size(ds)/8
  print *, "Ref: ",    storage_size(gr)/8

  v1 = Item_of(logic)
  v1 = Item_of(345597)
  print *, int4(v1)
  v1 = Item_of(34.55)
  print *, real4(v1)

  realVal = v1
  

  print *, is_valid(v1)
  print *, is_real4(v1)

  ti = content_type(v1)
  print *, ti%typeId

  v1 = Item_of(String('testinger'))
  v1 = 5.34
  v1 = Item_of(v1)
  v1 = String("testinger")
  v1 = 123
  v1 = 'bla & text'
  v1 = Item_of('bla & text')
  v1 = ref_of(gr)
  v1 = Item_of(ref_of(gr))

  v2 = v1
  v1 = v1

  v1 = 42
  v2 = 0

  print *, bool1(v1)
  print *, int1(v1)
  print *, int2(v1)
  print *, int4(v1)
  print *, int8(v1)
  print *, real4(v1)
  intvar = 0

  do i=1,500
    do j=1,500
      v2 = v1
      intvar = intvar + int4(v1)
    end do
  end do

  v2 = intvar
  intvar = v1

  gr2 = ref_of(intvar)
  gr  = ref_of(gr2)
  v1  = gr2

  do while (is_ref(gr))
    gr = ref(gr)
  end do
  v1 = int4(gr)

  gr = ref_of(v1)
  v2 = Item(gr)

  gr2 = clone(gr)
  v2  = Item(gr2)

  call free(gr2)

  call delete(ds)
  call delete(gr)
  call delete(gr2)
  call delete(v1)
  call delete(v2)


# define _item_type_(typeId, baseType) \
    call _paste(test_,typeId)();

  _TableOf_item_types_
# undef _item_type_

end


# define _nop(a)
# define _delete(a)   call delete(a)

# define _implementTest_(_typeId, _baseType, _val, _finish) \
  subroutine _paste(test_,_typeId)()   ;\
    use fde_item                       ;\
    use fde_ref                        ;\
    use fde_typeinfo                   ;\
    use fde_string                     ;\
    use iso_c_binding                  ;\
    implicit none                      ;\
    _baseType                 :: val   ;\
    _baseType,        pointer :: ptr   ;\
    type(Item_t)              :: vi    ;\
    type(TypeInfo_t), pointer :: ti    ;\
    complex*16, parameter :: cplx = (0.234,-3.4) ;\
    val = _val                         ;\
    vi  = Item_of(val)                 ;\
    vi  = Item_of(_val)                ;\
    vi  = cplx                         ;\
    vi  = _val                         ;\
    vi  = val                          ;\
    ptr => _typeId(vi)                 ;\
    val = vi                           ;\
    ti  => content_type(vi)            ;\
    print *, ti%typeId, ti%baseType    ;\
    print *, 'byteSize:', ti%typeSpecs%byteSize  ;\
    print *, _paste(is_,_typeId)( vi ) ;\
    print *, is_valid(vi)              ;\
    call delete( vi )                  ;\
    print *, is_valid(vi)              ;\
    _finish(val)                       ;\
  end subroutine

  _implementTest_(bool1,      logical*1, .true., _nop)
  _implementTest_(bool2,      logical*2, .true., _nop)
  _implementTest_(bool4,      logical*4, .true., _nop)
  _implementTest_(bool8,      logical*8, .true., _nop)
  _implementTest_(int1,       integer*1, 42, _nop)
  _implementTest_(int2,       integer*2, 42, _nop)
  _implementTest_(int4,       integer*4, 42, _nop)
  _implementTest_(int8,       integer*8, 42, _nop)
  _implementTest_(real4,      real*4, 1.5, _nop)
  _implementTest_(real8,      real*8, 1.5, _nop)
  _implementTest_(complex8,   complex*8, (1.5,2.5), _nop)
  _implementTest_(complex16,  complex*16, (1.5,2.5), _nop)
  _implementTest_(c_void_ptr, type(c_ptr), C_NULL_PTR, _nop)
  _implementTest_(string,     type(String_t), 'testinger string', _delete)
  _implementTest_(ref,        type(Ref_t), val, _delete)

# if defined ITEM_REAL16
  _implementTest_(real16,    real*16, 1.5, _nop)
  _implementTest_(complex32, complex*32, (1.5,2.5), _nop)
# endif

