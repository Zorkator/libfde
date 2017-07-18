
from ._object import Object
from ._ftypes import MemoryRef, mappedType, _mapType, POINTER_t
from ctypes  import c_int8, string_at, byref, c_char_p, c_int32
import six


class BaseString(Object):
  if six.PY2:
    __abstract__ = [basestring] #< use basestring as abstract base class

  _attribute_volatile  = c_int8(0)
  _attribute_permanent = c_int8(1)


@mappedType( 'string', 'type(String_t)', str )
class String(BaseString):

  @property
  def value( self ):
    m = MemoryRef()
    self.memoryref_( byref(m), byref(self) )
    return string_at( m.ptr, m.len )


  @value.setter
  def value( self, val ):
    if   isinstance( val, String ):
      self.assign_basestring_( byref(self), byref(val) )
    elif isinstance( val, six.string_types ):
      self.assign_charstring_( byref(self), c_char_p(val), c_int32(len(val)) )
    else:
      raise TypeError('a string type is required')


  def __init__( self, other = '' ):
    if isinstance( other, String ):
      super(String, self).__init__( other )
    else:
      other = str(other).encode('utf-8')
      self.init_by_charstring_( byref(self), byref(self._attribute_permanent), c_char_p(other), c_int32(len(other)) )


  def __repr__( self ):
    return "'%s'" % self.value

  def __str__( self ):
    if six.PY2: return self.value
    else      : return self.value.decode('utf-8')

  def __len__( self ):
    return self.len_( byref(self) )

  def encode( self, *args ):
    return self.value.encode( *args )

  def decode( self, *args ):
    return self.value.decode( *args )


StringPtr = POINTER_t(String)
_mapType( 'StringPtr', 'type(StringPtr_t)', StringPtr )

