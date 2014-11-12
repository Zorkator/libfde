
from ctypes  import *
from _base   import DynamicObject
from _ftypes import fortranType


@fortranType( 'type(HashMap_t)' )
class HashMap(DynamicObject):
  pass

