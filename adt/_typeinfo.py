
from ctypes  import *
from _base   import Compound
from _object import Object
from _ftypes import MemoryRef, _typeMap_ft2ct


class TypeSpecs(Structure):
  pass

TypeSpecs._fields_ = [('typeId',   MemoryRef),
                      ('baseType', MemoryRef),
                      ('byteSize', c_size_t),
                      ('rank',     c_size_t),
                      ('subtype',  POINTER(TypeSpecs))]


class TypeInfo(Compound):
  _fields_    = [('_spec', TypeSpecs)]
  _anonymous_ = ['_spec']

  @property
  def ctype( self ):
    """returns appropriate python ctype for this type"""
    cType = _typeMap_ft2ct[str(self.baseType)]
    sliceLen = self.byteSize / sizeof(cType)
    if sliceLen > 1: return cType * sliceLen
    else           : return cType

  def __str__( self ):
    what = ('scalar', '{4}-dimensional array, each', 'scalar')[bool(self.rank)]
    return '{0} <{1}>, {2} of {3} bytes'.format( self.typeId, self.baseType, what, self.byteSize, self.rank )



class TypedObject(Object):
  _fields_ = [('_typeInfo', POINTER(TypeInfo))]

  @property
  def ftype( self ):
    if self._typeInfo: return self._typeInfo.contents

  @property
  def ctype( self ):
    if self._typeInfo: return self._typeInfo.contents.ctype

  def __nonzero__( self ):
    return bool(self._typeInfo)

