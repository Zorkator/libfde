
from ctypes  import *
from _base   import Object, DynamicObject
from _ftypes import MemoryRef, _typeMap


class TypeSpecs(Structure):
  pass

TypeSpecs._fields_ = [('typeId',   MemoryRef),
                      ('baseType', MemoryRef),
                      ('byteSize', c_size_t),
                      ('rank',     c_size_t),
                      ('subtype',  POINTER(TypeSpecs))]


class TypeInfo(Object):
  _fields_    = [('_spec', TypeSpecs)]
  _anonymous_ = ['_spec']

  @property
  def ctype( self ):
    """returns appropriate python ctype for this type"""
    baseType = _typeMap[str(self.baseType)]
    sliceLen = self.byteSize / sizeof(baseType)
    if sliceLen > 1: return baseType * sliceLen
    else           : return baseType

  def __str__( self ):
    what = ('{0}-dimensional array, each', 'scalar')[bool(self.rank)]
    return '{0} <{1}>, {2} of {3} bytes'.format( self.typeId, self.baseType, what, self.byteSize )



class TypedObject(DynamicObject):
  _fields_ = [('typeInfo', POINTER(TypeInfo))]

