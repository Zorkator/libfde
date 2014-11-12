
from ctypes  import *
from _base   import DynamicObject
from _ftypes import fortranType


@fortranType( 'type(List_t)' )
class List(DynamicObject):
  pass

