
from ctypes  import *
from ._base   import Compound
from ._object import Object
from ._ftypes import MemoryRef, _typeMap_ft2ct


class TypeSpecs(Structure):
  pass

TypeSpecsPtr = POINTER(TypeSpecs)

TypeSpecs._fields_ = [('typeId',   MemoryRef),
                      ('baseType', MemoryRef),
                      ('byteSize', c_size_t),
                      ('rank',     c_size_t),
                      ('subtype',  TypeSpecsPtr)]


class TypeInfo(Compound):
  _fields_    = [('_spec', TypeSpecs)]
  _anonymous_ = ['_spec']

  @property
  def ctype( self ):
    """returns appropriate python ctype for this type"""
    cType = _typeMap_ft2ct[str(self.baseType)]
    sliceLen = self.byteSize // sizeof(cType)
    if sliceLen > 1: return cType * sliceLen
    else           : return cType

  def __str__( self ):
    fmt = ('{0} <{1}>, scalar of {2} bytes', '{0} <{1}>, {3}-dimensional array, each of {2} bytes')[bool(self.rank)]
    return fmt.format( self.typeId, self.baseType, self.byteSize, self.rank )

TypeInfoPtr = POINTER(TypeInfo)


class TypedObject(Object):
  __typeprocs__ = [] #< no native methods for TypedObject
  _fields_      = [('_typeInfo', TypeInfoPtr)]

  @property
  def ftype( self ):
    if self._typeInfo: return self._typeInfo.contents

  @property
  def ctype( self ):
    if self._typeInfo: return self._typeInfo.contents.ctype

  def __nonzero__( self ):
    return bool(self._typeInfo)

