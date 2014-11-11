
from ctypes  import *
from _base   import Object, DynamicObject
from _ftypes import MemoryRef


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



class TypedObject(DynamicObject):
  _fields_ = [('typeInfo', POINTER(TypeInfo))]

