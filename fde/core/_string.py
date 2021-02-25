
from ._object import Object
from ._ftypes import MemoryRef, mappedType, _mapType, POINTER_t
from ctypes   import c_int8, string_at, byref, c_char_p, c_int32
import six

#-------------------------------------------
class BaseString( Object ):
#-------------------------------------------
    if six.PY2:
        __abstract__ = [basestring] #< use basestring as abstract base class

    _attribute_volatile  = c_int8(0)
    _attribute_permanent = c_int8(1)

    @staticmethod
    def _encode( val, enc = 'utf-8' ):
        try   : return val.encode( enc )
        except: return val

    @staticmethod
    def _decode( val, enc = 'utf-8' ):
        try   : return val.decode( enc )
        except: return val


@mappedType( 'string', 'type(String_t)', str )
#-------------------------------------------
class String( BaseString ):
#-------------------------------------------

    @property
    def value( self ):
        m = MemoryRef()
        self.memoryref_( byref(m), byref(self) )
        return string_at( m.ptr, m.len )

    @value.setter
    def value( self, val ):
        if   isinstance( val, String ):
            self.assign_basestring_( byref(self), byref(val) )
        else:
            val = self._encode( val )
            self.assign_charstring_( byref(self), c_char_p( val ), c_int32(len(val)) )

    def __init__( self, other = '' ):
        if isinstance( other, String ):
            super( String, self ).__init__( other )
        else:
            other = self._encode( other )
            self.init_by_charstring_( byref(self), byref(self._attribute_permanent), c_char_p(other), c_int32(len(other)) )

    def __repr__( self ):
        return repr(self._decode( self.value ))

    def __str__( self ):
        return self._decode( self.value )

    def __len__( self ):
        return len(self._decode( self.value ))

    def encode( self, *args ):
        if args: return self.decode().encode( *args )
        else   : return self.value

    def decode( self, *args ):
        return self._decode( self.value, *args )


StringPtr = POINTER_t( String )
_mapType( 'StringPtr', 'type(StringPtr_t)', StringPtr )
