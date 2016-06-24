
from _hashmap import HashMap
from _ref     import Ref
from _ftypes  import mappedType, _mapType, CALLBACK
from ctypes   import byref, c_char_p, c_int, POINTER


_cbITF_buffer = dict()

@mappedType( 'hashmap', 'type(HashMap_t)' )
class Scope(HashMap):

  class Index(HashMap.Index):

    def get( self ):
      key, valRef = super(type(self), self).get()
      try   : return (key, valRef.typed.contents)
      except: return (key, valRef.typed)


  def __getitem__( self, ident ):
    return super(Scope, self).__getitem__( ident ).resolved


  def __setitem__( self, ident, val ):
    item   = self._getptr( self.get_, ident ).contents
    valRef = item.typed #< valRef := c_###() | Ref() | None [for NEW Items]

    if valRef is None: valRef = item                                  #< use Item for None-Type
    else             : valRef = getattr( valRef, 'contents', valRef ) #< use c_### or deref'ed Ref

    # valRef := c_###() [for Ref() | c_###()] | Item()
    try             : valRef[:]    = val[:]
    except TypeError: valRef.value = getattr( val, 'value', val )


  def get( self, ident, default = None ):
    ptr = self._getptr( self.get_ptr_, ident )
    if ptr: return ptr.contents.resolved
    else  : return default


  def resolve( self, path, default = None ):
    try:
      return reduce( lambda s,k: s[k], path, self )
    except (TypeError, KeyError):
      return default


  def setCallback( self, ident, cbFunc ):
    if type(type(cbFunc)) is not type(CALLBACK):
      _cbITF_buffer['{0}.{1}'.format(id(self), ident)] = func = CALLBACK(cbFunc or 0)
    return self.set_callback_( byref(self), c_char_p(ident), func, c_int(len(ident)) )


  @classmethod
  def getProcessScope( _class, *path ):
    ptr = POINTER(_class)()
    _class.__getattr__('get_processscope_')( byref(ptr) )
    # resolve scope nesting of given path ...
    return reduce( _class.__getitem__, path, ptr.contents )


_mapType( 'HashMapPtr', 'type(HashMapPtr_t)', POINTER(Scope) )

