
from ctypes  import *
from _object import Object, Compound
from _item   import Item
from _ftypes import MemoryRef, mappedType


@mappedType( 'hashmap', 'type(HashMap_t)' )
class HashMap(Object):

  class Index(Compound):
    __typename__ = 'HashMapIndex'

    def __nonzero__( self ):
      return self.is_valid_( byref(self) ) != 0

    def get( self ):
      keyRef, valPtr = MemoryRef(), POINTER(Item)()
      dummy = "{0}".format(valPtr) # de-optimize Python (????), so next line works
      self.item_( byref(keyRef), byref(valPtr), byref(self) )
      return (str(keyRef), valPtr.contents)

    def next( self ):
      if self.set_next_( byref(self) ) == 0:
        raise StopIteration


  def __init__( self, *args, **kwArgs ):
    super(HashMap, self).__init__()
    for k, v in dict( *args, **kwArgs ).items():
      self[k] = v

  def _getptr( self, getter, ident ):
    ptr = POINTER(Item)()
    dummy = "{0}".format(ptr) # de-optimize Python (????), so next line works
    getter( byref(ptr), byref(self), c_char_p(ident), c_int(len(ident)) )
    return ptr

  def __len__( self ):
    return self.len_( byref(self) )

  def clear( self ):
    self.clear_( byref(self) )

  def get( self, ident, default = None ):
    ptr = self._getptr( self.get_ptr_, ident )
    if ptr: return ptr.contents
    else  : return default

  def __getitem__( self, ident ):
    ptr = self._getptr( self.get_ptr_, ident )
    if ptr: return ptr.contents
    else  : raise KeyError(ident)

  def __setitem__( self, ident, val ):
    ptr = self._getptr( self.get_, ident )
    ptr.contents.value = val

  def __delitem__( self, ident ):
    self.remove_key_( byref(self), ident, len(ident) )

  def __contains__( self, ident ):
    return self.has_key_( byref(self), c_char_p(ident), c_int(len(ident)) ) != 0

  def __iter__( self ):
    for item in self.iteritems():
      yield item[0]

  def iteritems( self ):
    idx = self.Index()
    self.index_( byref(idx), byref(self) )
    while idx:
      yield idx.get()
      idx.next()

  def iterkeys( self ):
    for item in self.iteritems():
      yield item[0]

  def itervalues( self ):
    for item in self.iteritems():
      yield item[1]

  def items( self ):
    return list(self.iteritems())

  def keys( self ):
    return list(self.iterkeys())

  def values( self ):
    return list(self.itervalues())

  def setdefault( self, ident, default = None ):
    valPtr = POINTER(Item)()
    self.set_default_( byref(valPtr), byref(self), c_char_p(ident), byref(Item(default)), c_int(len(ident)) )
    return valPtr.contents

  def update( self, other = {}, **kwArgs ):
    if hasattr( other, 'keys' ):
      for k in other:
        self[k] = other[k]
    else:
      for k, v in other:
        self[k] = v

  def pop( self, key, default = Object.__metaclass__ ):
    valPtr = POINTER(Item)()
    self.pop_key_( byref(valPtr), byref(self), c_char_p(key), c_int(len(key)) )
    try   : return valPtr.contents
    except:
      if default is Object.__metaclass__:
        raise KeyError(key)
      else:
        return default




