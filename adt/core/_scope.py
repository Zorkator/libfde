
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
      return key, valRef.resolved

  ######################


  def setCallback( self, ident, cbFunc ):
    if type(type(cbFunc)) is not type(CALLBACK):
      _cbITF_buffer['{0}.{1}'.format(id(self), ident)] = func = CALLBACK(cbFunc or 0)
    return self.set_callback_( byref(self), c_char_p(ident), func, c_int(len(ident)) )


  def _assign_tree( self, other, keyOp=None ):
    keyOp = keyOp or (lambda k: k)

    def _tree_walk( itemItr, stack ):
      for key, v in itemItr:
        if not isinstance( v, CALLBACK ):
          k = keyOp( key )
          try                  : _tree_walk( v.iteritems(), stack + [stack[-1][k]] )
          except AttributeError: stack[-1][k] = v

    _tree_walk( other.iteritems(), [self] )


  def update( self, other = {}, **kwArgs ):
    self._assign_tree( other )
    self._assign_tree( kwArgs )


  def updateDomain( self, path_dict, keyOp=str.split ):
    self._assign_tree( path_dict, keyOp )


  def iterDomain( self, paths, keyOp=str.split ):
    keyOp = keyOp or (lambda k: [k])
    for p in map( keyOp, paths ):
      yield p, self[p] if p else None
    

  def extractDomain( self, paths, keyOp=str.split ):
    def _put( d, k ): return d.setdefault( k, dict() )

    domain = dict()
    for p, v in self.iterDomain( paths, keyOp ):
      if p: reduce( _put, p[:-1], domain )[p[-1]] = v
    return domain


  def extractContext( self, paths, keyOp=str.split ):
    return dict2obj( self.extractDomain( paths, keyOp ) )

  
  def asContext( self ):
    return dict2obj( self )


  def __getitem__( self, ident ):
    if isinstance( ident, (tuple, list) ):
      return reduce( lambda d,k: d[k], ident, self )
    else:
      return super(Scope, self).__getitem__( ident )


  def __setitem__( self, ident, val ):
    if isinstance( ident, (tuple, list) ):
      reduce( lambda d,k: d[k], ident[:-1], self )[ident[-1]] = val
    else:
      return super(Scope, self).__setitem__( ident, val )


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

