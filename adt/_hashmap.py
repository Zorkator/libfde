
from ctypes  import *
from _object import Object, Compound
from _item   import Item
from _ftypes import mappedType


@mappedType( 'hashmap', 'type(HashMap_t)' )
class HashMap(Object):

  def __getptr( self, getter, ident ):
    ptr = POINTER(Item)()
    dummy = "{0}".format(ptr) # de-optimize Python (????), so next line works
    getter( byref(ptr), byref(self), c_char_p(ident), c_int(len(ident)) )
    return ptr

  def __len__( self ):
    return self.len_( byref(self) )

  def clear( self ):
    self.clear_( byref(self) )

  def get( self, ident, default = None ):
    ptr = self.__getptr( self.get_ptr_, ident )
    if ptr: return ptr.contents
    else  : return default

  def __getitem__( self, ident ):
    ptr = self.__getptr( self.get_ptr_, ident )
    if ptr: return ptr.contents
    else  : raise KeyError(ident)

  def __setitem__( self, ident, val ):
    ptr = self.__getptr( self.get_, ident )
    ptr.contents.value = val



class HashMapIndex(Compound):
  pass

