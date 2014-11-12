
from ctypes    import *
from _typeinfo import TypedObject
from _ftypes   import fortranType, MemoryRef, Complex_16, _typeMap as _base2c
from _string   import String
from _ref      import Ref


@fortranType( 'type(Item_t)' )
class Item(TypedObject):
  
  @property
  def ctype( self ):
    if self.typeInfo: return _base2c[ str(self.typeInfo.contents.baseType) ]
    else            : return None

  @property
  def value( self ):
    ct = self.ctype
    if ct:
      mr = MemoryRef()
      self.memoryref_( byref(mr), byref(self) )
      return cast( mr.ptr, POINTER(ct) ).contents
    return None

  @value.setter
  def value( self, val ):
    try   : getattr( self, self._typeMap.get( type(val), '_set_pointer' ) )( val )
    except: raise TypeError('%s not supported' % type(val)) 

  
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
    self.assign_c_void_ptr_( byref(self), byref(val) )
    

Item._typeMap = {
  type(None) : '_set_none',
  bool       : '_set_bool',
  int        : '_set_int',
  long       : '_set_long',
  float      : '_set_float',
  complex    : '_set_complex',
  str        : '_set_charstring',
  String     : '_set_string',
  Ref        : '_set_ref',
  Item       : '_set_item' }

