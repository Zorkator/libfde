
from ctypes    import *
from _typeinfo import TypedObject, TypeSpecs
from _ftypes   import mappedType, _mapType, MemoryRef, Complex8, Complex16, Complex32
from _string   import String
from _ref      import Ref


@mappedType( 'item', 'type(Item_t)' )
class Item(TypedObject):

  UserAssignment = CFUNCTYPE( None, POINTER(c_void_p), POINTER(TypeSpecs), POINTER(c_void_p), POINTER(TypeSpecs) )

  @classmethod
  def onTypeMismatch( _class, func ):
    if type(type(func)) is not type(_class.UserAssignment):
      _class.__onTypeMismatch = func = _class.UserAssignment(func or 0)
    _class.__getattr__('on_type_mismatch_')( func )
  

  @TypedObject.value.getter
  def value( self ):
    ref = self.typed
    return getattr( ref, 'value', ref )

  @property
  def typed( self ):
    ct = self.ctype
    if ct:
      mr = MemoryRef()
      self.memoryref_( byref(mr), byref(self) )
      return cast( mr.ptr, POINTER(ct) ).contents

  @classmethod
  def assign_void_( _class, *args ):
    _class.delete_( *args )

  
  def __init__( self, val = None ):
    super(Item, self).__init__()
    if val is not None:
      self.value = val


_mapType( 'ItemPtr', 'type(ItemPtr_t)', POINTER(Item) )

