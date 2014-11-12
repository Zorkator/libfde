
from ctypes    import *
from _typeinfo import TypedObject
from _ftypes   import fortranType, MemoryRef, Complex_8, Complex_16, Complex_32
from _string   import String
from _ref      import Ref


@fortranType( 'type(Item_t)' )
class Item(TypedObject):
  
  @property
  def value( self ):
    ct = self.ftype.ctype
    if ct:
      mr = MemoryRef()
      self.memoryref_( byref(mr), byref(self) )
      obj = cast( mr.ptr, POINTER(ct) ).contents
      return getattr( obj, 'asRef', obj ).value
    return None

  @value.setter
  def value( self, val ):
    try   : getattr( self, self._typeMap.get( type(val), '_set_pointer' ) )( val )
    except: raise TypeError('%s not supported' % type(val)) 


  def __init__( self, val = None ):
    if val is not None:
      self.value = val
  
  def _set_none( self, val ):
    self.delete_( byref(self) )

  def _set_bool( self, val ):
    self.assign_bool1_( byref(self), byref(c_ubyte(val)) )

  def _set_int( self, val ):
    self.assign_int4_( byref(self), byref(c_long(val)) ) #< c_long is mapped to processor word
    
  def _set_long( self, val ):
    self.assign_int8_( byref(self), byref(c_int64(val)) )
    
  def _set_float( self, val ):
    self.assign_real8_( byref(self), byref(c_double(val)) )
    
  def _set_complex( self, val ):
    self.assign_complex16_( byref(self), byref(Complex_16(val)) )
    
  def _set_charstring( self, val ):
    self.assign_charstring_( byref(self), c_char_p(val), c_int32(len(val)) )
    
  def _set_string( self, val ):
    self.assign_string_( byref(self), byref(val) )
    
  def _set_ref( self, val ):
    self.assign_ref_( byref(self), byref(val) )
    
  def _set_item( self, val ):
    self.assign_item_( byref(self), byref(val) )
    
  def _set_pointer( self, val ):
    try     : ptr = val.ctypes._as_parameter_              #< try to get pointer of numpy object
    except AttributeError:
      try   : ptr = cast( byref(val), POINTER(type(val)) ) #< try ctypes array
      except: ptr = byref(val)                             #< some other scalar
    self.assign_c_void_ptr_( byref(self), ptr )
    
    

Item._typeMap = {
  type(None) : '_set_none',
  bool       : '_set_bool',
  int        : '_set_int',
  long       : '_set_long',
  float      : '_set_float',
  complex    : '_set_complex',
  Complex_8  : '_set_complex',
  Complex_16 : '_set_complex',
  Complex_32 : '_set_complex',
  str        : '_set_charstring',
  String     : '_set_string',
  Ref        : '_set_ref',
  Item       : '_set_item' }

