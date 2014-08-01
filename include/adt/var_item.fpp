#ifndef __VAR_ITEM_FPP
#define __VAR_ITEM_FPP

#include "adt/ppUtil.xpp"

# define _Table_varItem_types_ \
    _initType_(bool,     logical)               \
    _initType_(byte,     integer*1)             \
    _initType_(shortInt, integer*2)             \
    _initType_(int32,    integer*4)             \
    _initType_(longInt,  integer*8)             \
    _initType_(float,    real*4)                \
    _initType_(double,   real*8)                \
    _initType_(longDbl,  real*16)               \
    _initType_(cplx,     complex*8)             \
    _initType_(dblCplx,  complex*16)            \
    _initType_(quadCplx, complex*32)            \
    _initType_(cptr,     type(c_ptr))           \
    _initType_(string,   type(DynamicString_t)) \
    _initType_(gref,     type(GenericRef))

#endif

