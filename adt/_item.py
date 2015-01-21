
from ctypes    import *
from _typeinfo import TypedObject
from _ftypes   import mappedType, MemoryRef, Complex8, Complex16, Complex32
from _string   import String
from _ref      import Ref


@mappedType( 'item', 'type(Item_t)' )
class Item(TypedObject):
  
  @TypedObject.value.getter
  def value( self ):
    ct = self.ctype
    if ct:
      mr = MemoryRef()
      self.memoryref_( byref(mr), byref(self) )
      ref = cast( mr.ptr, POINTER(ct) ).contents
      return getattr( ref, 'value', ref )
    return None


  @classmethod
  def assign_void_( _class, *args ):
    _class.delete_( *args )

  
  def __init__( self, val = None ):
    super(Item, self).__init__()
    if val is not None:
      self.value = val

