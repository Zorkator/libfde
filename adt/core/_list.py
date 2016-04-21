
from ctypes  import *
from _object import Object, Compound
from _ftypes import mappedType, _mapType


@mappedType( 'list', 'type(List_t)' )
class List(Object):

  class Index(Compound):
    __typename__ = 'ListIndex'
    pass

  pass


_mapType( 'ListPtr', 'type(ListPtr_t)', POINTER(List) )

