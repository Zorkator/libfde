
from ._object import Object
from ._ftypes import MemoryRef, mappedType, _mapType, POINTER_t
from ctypes   import c_int8, string_at, byref, c_char_p, c_int32

#-------------------------------------------
class BaseString( Object ):
#-------------------------------------------
    try   :  __abstract__ = [basestring] #< py2: use basestring as abstract base class
    except: pass

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
    def bytes( self ):
        m = MemoryRef()
        self.memoryref_( byref( m ), byref( self ) )
        return string_at( m.ptr, m.len )

    @property
    def value( self ):
        if bytes is str: return self.bytes          #< py2 return str (byte string)
        else           : return self.bytes.decode() #< py3 return decoded to str

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
        return repr(self.value)

    def __str__( self ):
        return self.value

    def __len__( self ):
        return len(self.value)

    def __getitem__( self, key ):
        return self.value[key]

    def encode( self, *args ):
        return self.value.encode( *args )

    def decode( self, *args ):
        return self.bytes.decode( *args )


StringPtr = POINTER_t( String )
_mapType( 'StringPtr', 'type(StringPtr_t)', StringPtr )
