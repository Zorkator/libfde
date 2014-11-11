
from ctypes  import *
from _base   import DynamicObject
from _ftypes import MemoryRef


class BaseString(DynamicObject):
  _attribute_volatile  = c_int8(0)
  _attribute_permanent = c_int8(1)



class String(BaseString):
  def __init__( self, s = '' ):
    self.init_by_charstring_( byref(self), byref(self._attribute_permanent), c_char_p(s), c_int32(len(s)) )
  
  def __str__( self ):
    m = MemoryRef()
    self.memoryref_( byref(m), byref(self) )
    return string_at( m.ptr, m.len )

  def __len__( self ):
    return self.len_( byref(self) )

  def assign( self, other ):
    if isinstance( other, String ): self.assign_string_( byref(self), byref(other) )
    else                          : self.assign_


