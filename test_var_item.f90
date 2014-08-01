
#include "adt/var_item.fpp"

program testinger
  use var_item
  use generic_ref
  use dynamic_string
  use type_references
  implicit none

  type(VarItem_t)       :: v1, v2
  type(TypeInfo)        :: ti
  integer*4             :: intvar
  type(DynamicString_t) :: ds
  type(GenericRef)      :: gr, gr2

  print *, "VarItem: ",       storage_size(v1)/8
  print *, "DynamicString: ", storage_size(ds)/8
  print *, "GenericRef: ",    storage_size(gr)/8

  v1 = VarItem(345597)
  print *, int32(v1)
  v1 = VarItem(34.55)
  print *, float(v1)

  print *, is_valid(v1)
  print *, is_float(v1)

  ti = typeinfo_of(v1)
  print *, ti%typeId

  v1 = VarItem(DynamicString('testinger'))
  v1 = 5.34
  v1 = DynamicString("testinger")
  v1 = 'bla & text'
  v1 = VarItem('bla & text')
  v1 = ref(gr)

  v2 = v1
  v1 = v1

  v1 = 42

  intvar = v1

  gr2 = ref(intvar)
  gr  = ref(gr2)

  do while (is_ref(gr))
    gr = deref(gr)
  end do
  v1 = int32(gr)

  call delete(ds)
  call delete(gr)
  call delete(gr2)
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
    _baseType               :: val     ;\
    _baseType,      pointer :: ptr     ;\
    type(VarItem_t)         :: vi      ;\
    type(TypeInfo), pointer :: ti      ;\
    vi  = VarItem(val)                 ;\
    vi  = val                          ;\
    ptr => _typeId(vi)                 ;\
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
  _implementTest_(int32,    integer*4, _nop)
  _implementTest_(longInt,  integer*8, _nop)
  _implementTest_(float,    real*4, _nop)
  _implementTest_(double,   real*8, _nop)
  _implementTest_(longDbl,  real*16, _nop)
  _implementTest_(cplx,     complex*8, _nop)
  _implementTest_(dblCplx,  complex*16, _nop)
  _implementTest_(quadCplx, complex*32, _nop)
  _implementTest_(cptr,     type(c_ptr), _nop)
  _implementTest_(string,   type(DynamicString_t), _delete)
  _implementTest_(gref,     type(GenericRef), _delete)

