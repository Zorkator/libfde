
from ctypes  import *
from _object import Object
from _ftypes import MemoryRef, mappedType


class BaseString(Object):
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
    if isinstance( val, String ):
      self.assign_basestring_( byref(self), byref(other) )
    else:
      val = str(val)
      self.assign_charstring_( byref(self), c_char_p(val), c_int32(len(val)) )

  def __init__( self, other = '' ):
    if isinstance( other, String ):
      super(String, self).__init__( other )
    else:
      other = str(other)
      self.init_by_charstring_( byref(self), byref(self._attribute_permanent), c_char_p(other), c_int32(len(other)) )

  @property
  def contents( self ):
    return self.value

  def __str__( self ):
    return self.value

  def __len__( self ):
    return self.len_( byref(self) )

