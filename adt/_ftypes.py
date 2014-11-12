
from ctypes import *

_f2c_typeMap = {
  "logical*1"        : c_uint8,
  "logical*2"        : c_uint16,
  "logical*4"        : c_uint32,
  "logical*8"        : c_uint64,
  "integer*1"        : c_int8,
  "integer*2"        : c_int16,
  "integer*4"        : c_int32,
  "integer*8"        : c_int64,
  "real*4"           : c_float,
  "real*8"           : c_double,
  "real*16"          : c_longdouble,
  "type(c_ptr)"      : c_void_p,
  "character(*)"     : c_char }


def fortranType( ident, ctypesClass = None ):
  def classOp( _class ):
    global _f2c_typeMap
    _f2c_typeMap[ident] = ctypesClass or _class
    return _class
  return classOp


def _ComplexType( name, ftype, ctype ):
  def _init( self, *args ):
    cplx = complex(*args)
    super(self.__class__, self).__init__( cplx.real, cplx.imag )

  def _cplx( self ):
    return complex( self.real, self.imag )

  members = dict( _fields_    = [('real', ctype), ('imag', ctype)],
                  __init__    = _init,
                  __complex__ = _cplx,
                  value       = property( _cplx )
                )
  _class  = type( name, (Structure,), members )
  _f2c_typeMap[ftype] = _class
  globals()[name]     = _class
  return _f2c_typeMap


_ComplexType( 'Complex_8',  'complex*8',  c_float )
_ComplexType( 'Complex_16', 'complex*16', c_double )
_ComplexType( 'Complex_32', 'complex*32', c_longdouble )


class MemoryRef(Structure):
  _fields_ = [('ptr', c_void_p),
              ('len', c_size_t)]

  def __str__( self ):
    return string_at( self.ptr, self.len )

  def __nonzero__( self ):
    return self.len and self.ptr

