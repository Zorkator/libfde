
from ctypes   import c_int32
from ._base   import Compound
from ._ftypes import _typeMap_py2id as _id_map, _typeMap_py2ct as _ct_map


######################################
class Object(Compound):
######################################
    __typeprocs__ = [] #< no native methods for object

    @property
    def value( self ):
        return self

    @value.setter
    def value( self, val ):
        _call = self._lookup_method( 'assign', type(val) )
        _ctyp = _ct_map.lookup( type(val) )
        _cval = val if isinstance( val, _ctyp ) else _ctyp(val)
        _call( byref(self), byref(_cval) )


    def _lookup_method( self, name, _type ):
        try   : return getattr( self, '{0}_{1}_'.format( name, _id_map.lookup(_type) ) )
        except: raise TypeError( "%s.%s doesn't support %s" % (type(self).__name__, name, _type) )


    def __init__( self, other = None ):
        proto = self if other is None else other
        _call = self._lookup_method( 'init_by', type(proto) )

        if proto is self:
            _call( byref(self), byref(c_int32(0)) )
        else:
            _ctyp = _ct_map.lookup( type(proto) )
            _call( byref(self), byref(c_int32(1)), byref(_ctyp(proto)) )


    def delete( self ):
        self.delete_( byref(self) )


