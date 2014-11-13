
from ctypes  import *
from _base   import DynamicObject
from _ftypes import MemoryRef, fortranType


class BaseString(DynamicObject):
  _attribute_volatile  = c_int8(0)
  _attribute_permanent = c_int8(1)


@fortranType( 'type(String_t)' )
class String(BaseString):
  
  @property
  def value( self ):
    m = MemoryRef()
    self.memoryref_( byref(m), byref(self) )
    return string_at( m.ptr, m.len )

  @value.setter
  def value( self, val ):
    if   isinstance( val, String ): self.assign_basestring_( byref(self), byref(other) )
    elif isinstance( val, str )   : self.assign_charstring_( byref(self), c_char_p(val), c_int32(len(val)) )

  def __init__( self, other = '' ):
    if isinstance( other, basestring ):
      self.init_by_charstring_( byref(self), byref(self._attribute_permanent), c_char_p(other), c_int32(len(other)) )
    else:
      super(String, self).__init__( other ) 

  def __str__( self ):
    return self.value

  def __len__( self ):
    return self.len_( byref(self) )

