
from ctypes  import *
from _object import Object, Compound
from _ftypes import mappedType, _mapType, POINTER_t


@mappedType( 'list', 'type(List_t)' )
class List(Object):

  class Index(Compound):
    __typename__ = 'ListIndex'
    pass

  pass


ListPtr = POINTER_t(List)
_mapType( 'ListPtr', 'type(ListPtr_t)', ListPtr )

