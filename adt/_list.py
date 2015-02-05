
from ctypes  import *
from _object import Object, Compound
from _ftypes import mappedType


@mappedType( 'list', 'type(List_t)' )
class List(Object):
  pass



class ListIndex(Compound):
  pass

