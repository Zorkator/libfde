
from ctypes  import *
from ._object import Object, Compound
from ._item   import Item, ItemPtr
from ._ftypes import MemoryRef, mappedType, _mapType, POINTER_t
from ..tools  import auto_raise


@mappedType( 'hashmap', 'type(HashMap_t)' )
class HashMap(Object):
  __serialize_as__ = dict

  class Index(Compound):
    __typename__ = 'HashMapIndex'

    def __nonzero__( self ):
      return self.is_valid_( byref(self) ) != 0


    def get( self ):
      keyRef = MemoryRef()
      ptr    = ItemPtr(); bool(ptr) #< de-optimize Python (????), so next line works
      self.item_( byref(keyRef), byref(ptr), byref(self) )
      return (str(keyRef), ptr.contents)


    def next( self ):
      if self.set_next_( byref(self) ) == 0:
        raise StopIteration

  ######################

  @property
  def parent( self ):
    ptr = POINTER_t(type(self))()
    self.parent_( byref(ptr), byref(self) )
    if ptr: return ptr.contents
    else  : return None


  @classmethod
  def _walk( _class, scope, level = 1 ):
    keys  = sorted( scope.keys() )
    width = max( map( len, keys ) or [0] )
    for k in keys:
      v = scope[k]
      yield (level, width, k, v)
      try:
        for _ in _class._walk( v, level + 1 ):
          yield _
      except: pass


  def __init__( self, *args, **kwArgs ):
    super(HashMap, self).__init__()
    for k, v in dict( *args, **kwArgs ).items():
      self[k] = v


  def _getptr( self, getter, key ):
    ptr = ItemPtr(); bool(ptr) #< de-optimize Python (????), so next line works
    key = key.encode('utf-8')
    getter( byref(ptr), byref(self), c_char_p(key), c_int(len(key)) )
    return ptr


  def _format( self, level = 1, indent = '\t' ):
    for lvl, keyWdth, key, val in self._walk( self, level ):
      yield '%s%-*s : %s' % (indent * lvl, keyWdth, key, repr(val))


  def __len__( self ):
    return self.len_( byref(self) )


  def __repr__( self ):
    return "%s %s [%d]" % (type(self).__name__, hex(id(self)), len(self))


  def __str__( self ):
    return '\n'.join( self._format() )


  def clear( self ):
    self.clear_( byref(self) )


  def getItem( self, ident, default = None ):
    ptr = self._getptr( self.get_ptr_, ident )
    if ptr: return ptr.contents
    return auto_raise( default, ident )


  def get( self, ident, default = None ):
    ptr = self._getptr( self.get_ptr_, ident )
    if ptr: return ptr.contents.resolved
    else  : return default


  def __getitem__( self, ident ):
    ptr = self._getptr( self.get_ptr_, ident )
    if ptr: return ptr.contents.resolved
    else  : raise KeyError(ident)


  def __setitem__( self, ident, val ):
    self._getptr( self.get_, ident ).contents.resolved = val


  def __delitem__( self, ident ):
    self.remove_key_( byref(self), ident, len(ident) )


  def __contains__( self, ident ):
    return self.has_key_( byref(self), c_char_p(ident), c_int(len(ident)) ) != 0


  def __iter__( self ):
    for item in self.iteritems( HashMap.Index ):
      yield item[0]


  def iteritems( self, indexBy=None ):
    idx = (indexBy or self.Index)()
    self.index_( byref(idx), byref(self) )
    while idx:
      yield idx.get()
      idx.next()


  def iterkeys( self ):
    for item in self.iteritems( HashMap.Index ):
      yield item[0]


  def itervalues( self ):
    for item in self.iteritems():
      yield item[1]


  def items( self, indexBy=None ):
    return list(self.iteritems( indexBy ))


  def keys( self ):
    return list(self.iterkeys())


  def values( self ):
    return list(self.itervalues())


  def setdefault( self, ident, default = None ):
    ptr = ItemPtr()
    self.set_default_( byref(ptr), byref(self), c_char_p(ident), byref(Item(default)), c_int(len(ident)) )
    return ptr.contents.resolved


  def update( self, other = {}, **kwArgs ):
    if hasattr( other, 'keys' ):
      for k in other:
        self[k] = other[k]
    else:
      for k, v in other:
        self[k] = v
    for k, v in kwArgs.items():
      self[k] = v


  def pop( self, key, default = KeyError ):
    ptr = ItemPtr()
    self.pop_key_( byref(ptr), byref(self), c_char_p(key), c_int(len(key)) )
    try   : return ptr.contents.resolved
    except: return auto_raise( default, key )



HashMapPtr = POINTER_t(HashMap)
_mapType( 'HashMapPtr', 'type(HashMapPtr_t)', HashMapPtr )

