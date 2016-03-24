
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
    valRef = super(Scope, self).__getitem__( ident ).typed
    try              : return valRef.contents
    except ValueError: return POINTER(c_int)()
    except           : return valRef


  def __setitem__( self, ident, val ):
    item   = self._getptr( self.get_, ident ).contents
    valRef = item.typed #< valRef := c_###() | Ref() | None [for NEW Items]

    if valRef is None: valRef = item                                  #< use Item for None-Type
    else             : valRef = getattr( valRef, 'contents', valRef ) #< use c_### or deref'ed Ref

    # valRef := c_###() [for Ref() | c_###()] | Item()
    try             : valRef[:]    = val[:]
    except TypeError: valRef.value = getattr( val, 'value', val )


  def setCallback( self, ident, cbFunc ):
    if type(type(cbFunc)) is not type(CALLBACK):
      _cbITF_buffer['{0}.{1}'.format(id(self), ident)] = func = CALLBACK(cbFunc or 0)
    return self.set_callback_( byref(self), c_char_p(ident), func, c_int(len(ident)) )


  @classmethod
  def getProcessScope( _class, *path ):
    ptr = POINTER(_class)()
    _class.__getattr__('get_processscope_')( byref(ptr) )
    scope = ptr.contents
    # resolve scope nesting of given path ...
    for p in path:
      scope = scope[p]
    return scope


_mapType( 'HashMapPtr', 'type(HashMapPtr_t)', POINTER(Scope) )

