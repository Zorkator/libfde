
from ctypes import *

#_typeMap_f2py = {
#  "logical*1"        : c_uint8,
#  "logical*2"        : c_uint16,
#  "logical*4"        : c_uint32,
#  "logical*8"        : c_uint64,
#  "integer*1"        : c_int8,
#  "integer*2"        : c_int16,
#  "integer*4"        : c_int32,
#  "integer*8"        : c_int64,
#  "real*4"           : c_float,
#  "real*8"           : c_double,
#  "real*16"          : c_longdouble,
#  "type(c_ptr)"      : c_void_p,
#  "character(*)"     : c_char }
#
#_typeMap_py2f = {
#  bool  : "bool1",
#  int   : "int4",
#  long  :  "int8",
#  float : "real8",
#  str   : "charstring" }

_typeMap_py2id = {} #< call
_typeMap_py2ct = {} #< value conversion
_typeMap_ft2ct = {} #< value read (cast MemoryRef to value pointer)


def _mapType( typeId, ftype, ctype, *pyTypes ):
  global _typeMap_py2id, _typeMap_py2ct, _typeMap_ft2ct
  _typeMap_ft2ct[ftype] = ctype
  for py in pyTypes + (ctype,):
    _typeMap_py2id[py] = typeId
    _typeMap_py2ct[py] = ctype
  

def mappedType( typeId, ftype, *pyTypes ):
  def _wrap( ctype ):
    _mapType( typeId, ftype, ctype, *pyTypes )
    return ctype
  return _wrap


_mapType( 'bool1', 'logical*1', c_uint8, bool )
_mapType( 'bool2', 'logical*2', c_uint16 )
_mapType( 'bool4', 'logical*4', c_uint32 )
_mapType( 'bool8', 'logical*8', c_uint64 )

_mapType( 'int1',  'integer*1', c_int8 )
_mapType( 'int2',  'integer*2', c_int16 )
_mapType( 'int4',  'integer*4', c_int32, int )
_mapType( 'int8',  'integer*8', c_int64 )

_mapType( 'real4',  'real*4', c_float )
_mapType( 'real8',  'real*8', c_double, float )


#@mappedType( 'complex16', 'complex*16', assignable = (complex,) )
#class Complex(Structure):
#  _fields_ = [('_data', c_double*2)]
#
#
#
#def fortranType( ident, ctypesClass = None ):
#  def classOp( _class ):
#    global _typeMap_f2py
#    _typeMap_f2py[ident] = ctypesClass or _class
#    return _class
#  return classOp
#

def _complexType( typeId, ftype, compType, *pyTypes ):
  def _setVal( self, *args ):
    cplx = complex(*args)
    super(self.__class__, self).__init__( cplx.real, cplx.imag )

  def _getVal( self ):
    return complex( self.real, self.imag )

  members = dict( _fields_    = [('real', compType), ('imag', compType)],
                  __init__    = _setVal,
                  __complex__ = _getVal,
                  value       = property( _getVal, _setVal )
                )
  classId = typeId.capitalize()
  _class  = type( classId, (Structure,), members )
  _mapType( typeId, ftype, _class, *pyTypes )
  globals()[classId] = _class
  return _class


_complexType( 'complex8',  'complex*8',  c_float )
_complexType( 'complex16', 'complex*16', c_double, complex )
_complexType( 'complex32', 'complex*32', c_longdouble )


class MemoryRef(Structure):
  _fields_ = [('ptr', c_void_p),
              ('len', c_size_t)]

  def __str__( self ):
    return string_at( self.ptr, self.len )

  def __nonzero__( self ):
    return self.len and self.ptr

