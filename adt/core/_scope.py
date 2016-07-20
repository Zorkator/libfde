
from _hashmap import HashMap
from _ref     import Ref
from _ftypes  import mappedType, _mapType, CALLBACK, POINTER_t
from ..tools  import dict2obj
from ctypes   import byref, c_char_p, c_int, POINTER


_cbITF_buffer = dict()

@mappedType( 'hashmap', 'type(HashMap_t)' )
class Scope(HashMap):

  class Index(HashMap.Index):

    def get( self ):
      key, valRef = super(type(self), self).get()
      try   : return (key, valRef.typed.contents)
      except: return (key, valRef.typed)

  ######################


  def resolve( self, path, default = None ):
    try  : return reduce( lambda s,k: s[k], path, self )
    except (TypeError, KeyError):
      if default is Exception: raise
      else                   : return default


  def setCallback( self, ident, cbFunc ):
    if type(type(cbFunc)) is not type(CALLBACK):
      _cbITF_buffer['{0}.{1}'.format(id(self), ident)] = func = CALLBACK(cbFunc or 0)
    return self.set_callback_( byref(self), c_char_p(ident), func, c_int(len(ident)) )


  def extract( self, paths, keySep=str ):
    if keySep is str: op = lambda k: [k]
    else            : op = lambda k: k.split(keySep)

    def _put( d, k ): return d.setdefault( k, dict() )
    def _get( d, k ): return d[k]

    ctxt = dict()
    for p in filter( bool, map( op, paths )):
      reduce( _put, p[:-1], ctxt )[p[-1]] = reduce( _get, p, self )
    return ctxt


  def extractContext( self, paths, keySep=str ):
    return dict2obj( self.extract( paths, keySep ) )

  
  def asContext( self ):
    return dict2obj( self )


  @classmethod
  def getProcessScope( _class, *path ):
    ptr = POINTER(_class)()
    _class.__getattr__('get_processscope_')( byref(ptr) )
    # resolve scope nesting of given path ...
    return reduce( _class.__getitem__, path, ptr.contents )


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


  def _format( self, level = 1, indent = '\t' ):
    for lvl, keyWdth, key, val in self._walk( self, level ):
      yield '%s%-*s : %s' % (indent * lvl, keyWdth, key, repr(val))


  def __repr__( self ):
    return "%s %s [%d]" % (type(self).__name__, hex(id(self)), len(self))


  def __str__( self ):
    return '\n'.join( self._format() )




ScopePtr = POINTER_t(Scope)
_mapType( 'HashMapPtr', 'type(HashMapPtr_t)', ScopePtr )

