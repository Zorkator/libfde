
from ctypes  import *
from _object import Object
from _ftypes import mappedType


@mappedType( 'list', 'type(List_t)' )
class List(Object):
  pass

