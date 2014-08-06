#ifndef __VAR_ITEM_FPP
#define __VAR_ITEM_FPP

#include "adt/ppUtil.xpp"

# define _Table_varItem_types_ \
    _initType_(bool,       logical)               \
    _initType_(int8,       integer*1)             \
    _initType_(int16,      integer*2)             \
    _initType_(int32,      integer*4)             \
    _initType_(int64,      integer*8)             \
    _initType_(real32,     real*4)                \
    _initType_(real64,     real*8)                \
    _initType_(real128,    real*16)               \
    _initType_(complex32,  complex*8)             \
    _initType_(complex64,  complex*16)            \
    _initType_(complex128, complex*32)            \
    _initType_(c_void_ptr, type(c_ptr))           \
    _initType_(string,     type(DynamicString_t)) \
    _initType_(gref,       type(GenericRef_t))

#endif

