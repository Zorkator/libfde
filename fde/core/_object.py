
from ctypes  import *
from ._base   import Compound
from ._ftypes import _typeMap_py2id as _id_map, _typeMap_py2ct as _ct_map


######################################
class Object(Compound):
######################################
    __typeprocs__ = [] #< no native methods for object
    __slots__     = ['_needs_delete', '_py_data']
    __py_cache__  = dict()

    @property
    def value( self ):
        return self

    @value.setter
    def value( self, val ):
        _call = self._lookup_method( 'assign', type(val) )
        _ctyp = _ct_map.lookup( type(val) )
        _cval = val if isinstance( val, _ctyp ) else _ctyp(val)
        _call( byref(self), byref(_cval) )

    @property
    def pyData( self ):
        try   : return self._py_data
        except:
            self._py_data = Object.__py_cache__.setdefault( addressof(self), dict() )
            return self._py_data


    def _lookup_method( self, name, _type ):
        try   : return getattr( self, '{0}_{1}_'.format( name, _id_map.lookup(_type) ) )
        except: raise TypeError( "%s.%s doesn't support %s" % (type(self).__name__, name, _type) )


    def __new__( _class, *args, **kwArgs ):
        self = super(Object, _class).__new__( _class, *args, **kwArgs )
        self._needs_delete = True
        return self


    def __init__( self, other = None ):
        proto = self if other is None else other
        _call = self._lookup_method( 'init_by', type(proto) )

        if proto is self:
            _call( byref(self), byref(c_int32(0)) )
        else:
            _ctyp = _ct_map.lookup( type(proto) )
            _call( byref(self), byref(c_int32(1)), byref(_ctyp(proto)) )


    def __del__( self ):
        if self._needs_delete:
            self.delete()


    def delete( self ):
        Object.__py_cache__.pop( addressof(self), None )
        self.delete_( byref(self) )

