
from ctypes   import *
from ._object import Object, Compound
from ._ftypes import mappedType, _mapType, POINTER_t

@mappedType( 'list', 'type(List_t)' )
#-------------------------------------------
class List( Object ):
#-------------------------------------------

    #-------------------------------------------
    class Index( Compound ):
    #-------------------------------------------
        __typename__ = 'ListIndex'

        def __bool__( self ):
            return self.is_valid_( byref(self) ) != 0
        __nonzero__ = __bool__  # < py2 compatibility


    def __len__( self ):
        return self.len_( byref(self) )

    def __repr__( self ):
        return "%s %s [%d]" % (type(self).__name__, hex(id(self)), len(self))

    def clear( self ):
        self.clear_( byref( self ) )


ListPtr = POINTER_t( List )
_mapType( 'ListPtr', 'type(ListPtr_t)', ListPtr )
