
from ctypes    import *
from _typeinfo import TypedObject, TypeSpecsPtr
from _ftypes   import mappedType, _mapType, MemoryRef, Complex8, Complex16, Complex32, VOID_Ptr, POINTER_t
from _string   import String
from _ref      import Ref


@mappedType( 'item', 'type(Item_t)' )
class Item(TypedObject):

  UserAssignment = CFUNCTYPE( None, VOID_Ptr, TypeSpecsPtr, VOID_Ptr, TypeSpecsPtr )

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


  @property
  def resolved( self ):
    tgt = self.typed
    try              : return tgt.contents #< referenced
    except ValueError: return tgt._type_() #< null-pointer access
    except           : return tgt

  
  @resolved.setter
  def resolved( self, val ):
    tgt = self.typed
    if tgt is None: tgt = self                            #< use Item for None-Type
    else          : tgt = getattr( tgt, 'contents', tgt ) #< use c_### or deref'ed Ref

    # tgt := c_###() [for Ref() | c_###()] | Item()
    try             : tgt[:]    = val[:]
    except TypeError: tgt.value = getattr( val, 'value', val )


  @classmethod
  def assign_void_( _class, *args ):
    _class.delete_( *args )

  
  def __init__( self, val = None ):
    super(Item, self).__init__()
    if val is not None:
      self.value = val


ItemPtr = POINTER_t(Item)
_mapType( 'ItemPtr', 'type(ItemPtr_t)', ItemPtr )

