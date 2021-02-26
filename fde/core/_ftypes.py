
from ctypes   import *
from operator import mul as _mul
import six

try:
    from functools import reduce
except ImportError:
    pass

#-------------------------------------------
class MemoryRef( Structure ):
#-------------------------------------------
    _fields_ = [('ptr', c_void_p),
                ('len', c_size_t)]

    def __str__( self ):
        if six.PY2: return string_at( self.ptr, self.len )
        else      : return string_at( self.ptr, self.len ).decode( 'utf-8' )

    def __nonzero__( self ):
        return self.len > 0 and bool(self.ptr)


#-------------------------------------------
class TypeMap( dict ):
#-------------------------------------------

    def lookup( self, val ):
        return self.get( val ) or self[type(val)]


_typeMap_py2id = TypeMap() #< call type mangling
_typeMap_py2ct = TypeMap() #< value conversion
_typeMap_ft2ct = TypeMap() #< value read (cast MemoryRef to value pointer)


def _mapType( typeId, ftype, ctype, *pyTypes ):
    _typeMap_ft2ct[ftype] = ctype
    for py in pyTypes + (ctype,):
        _typeMap_py2id[py] = typeId
        _typeMap_py2ct[py] = ctype


def mappedType( typeId, ftype, *pyTypes ):
    def _wrap( ctype ):
        _mapType( typeId, ftype, ctype, *pyTypes )
        return ctype
    return _wrap


def _complexType( typeId, ftype, compType, *pyTypes ):
    def _setVal( self, *args ):
        cplx = complex( *args )
        super(self.__class__, self).__init__( cplx.real, cplx.imag )

    def _getVal( self ):
        return complex( self.real, self.imag )

    def _repr( self ):
        return str( self.value )

    members = dict( _fields_    = [('real', compType), ('imag', compType)],
                    __init__    = _setVal,
                    __complex__ = _getVal,
                    __repr__    = _repr,
                    value       = property( _getVal, _setVal )
                  )
    classId = typeId.capitalize()
    _class  = type( classId, (Structure,), members )
    _mapType( typeId, ftype, _class, *pyTypes )
    globals()[classId] = _class
    return _class


_mapType( 'void', '', c_void_p, type(None) )

_mapType( 'bool1', 'logical*1', c_uint8, bool )
_mapType( 'bool2', 'logical*2', c_uint16 )
_mapType( 'bool4', 'logical*4', c_uint32 )
_mapType( 'bool8', 'logical*8', c_uint64 )

_mapType( 'int1',  'integer*1', c_int8 )
_mapType( 'int2',  'integer*2', c_int16 )
_mapType( 'int4',  'integer*4', c_int32, int )
_mapType( 'int8',  'integer*8', c_int64 )

_mapType( 'real4',  'real*4',  c_float )
_mapType( 'real8',  'real*8',  c_double, float )
_mapType( 'real16', 'real*16', c_longdouble )

_complexType( 'complex8',  'complex*8',  c_float )
_complexType( 'complex16', 'complex*16', c_double, complex )
_complexType( 'complex32', 'complex*32', c_longdouble )


##
# some helper functions for array and pointer types
#

def _ptr_repr( self ):
    if self: return 'ptr(%s)' % self._type_.__name__
    else   : return 'null'


def _func_repr( self ):
    if self: return 'ptr(%s)' % type(self).__name__
    else   : return 'None'


def _array_raw( self ):
    return cast( byref(self), self._rawtype_ ).contents


def _array_repr( self ):
    return 'Array(%s, %s): %s' % (self._basetype_.__name__, self._shape_, self.raw[:])


def ARRAY_t( base, shape ):
    shp = list( map( int, shape ) )
    cls = reduce( _mul, shp, base )
    cls._shape_    = shp
    cls._basetype_ = base
    cls._rawtype_  = POINTER( base * (sizeof(cls) // sizeof(base)) )
    cls.__repr__   = _array_repr
    cls.raw        = property( _array_raw )
    return cls


def POINTER_t( tgtType ):
    T = POINTER( tgtType )
    T.__repr__ = _ptr_repr
    return T


def CALLBACK_t( argType = None ):
    if argType is None: T = CFUNCTYPE( None )
    else              : T = CFUNCTYPE( None, POINTER(argType) )
    T.__repr__ = _func_repr
    return T


def CFUNCTION_t( retType = None, *args ):
    T = CFUNCTYPE( retType, *args )
    T.__repr__ = _func_repr
    return T


VOID_Ptr = POINTER_t( c_void_p )
CALLBACK = CALLBACK_t()
SHORT_ID = c_char * 10 #< define this type for gfortran workaround

# Override representation of pointer types
# ctypes doesn't like deriving those classes.
SHORT_ID.__repr__ = lambda self: "'%s'" % self[:]


_mapType( 'c_void_ptr', 'type(c_ptr)',             VOID_Ptr, type(VOID_Ptr) )
_mapType( 'Callback',   'procedure(Callback_itf)', CALLBACK )
_mapType( 'char10',     'character(len=10)',       SHORT_ID )
