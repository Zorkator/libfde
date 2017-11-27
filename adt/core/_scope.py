
from ._hashmap import HashMap
from ._ref     import Ref
from ._ftypes  import mappedType, _mapType, CALLBACK, CALLBACK_t, POINTER_t, VOID_Ptr
from ..tools  import dict2obj
from ctypes   import byref, c_char_p, c_int, POINTER, sizeof, Array

try:
  from functools import reduce
except ImportError:
  pass


@mappedType( 'hashmap', 'type(HashMap_t)' )
class Scope(HashMap):

  class Index(HashMap.Index):

    def get( self ):
      key, valRef = super(type(self), self).get()
      return key, valRef.resolved

  ######################

  def declareCallback( self, ident, argType = None ):
    ident = ident.encode()
    self.declare_callback_( byref(self), c_char_p(ident), c_int(len(ident)) )
    self[ident].pyData['itf'] = CALLBACK_t(argType)


  def typifyCallback( self, ident, argType ):
    ident = ident.encode()
    self[ident].pyData['itf'] = CALLBACK_t(argType)


  def _mk_CALLBACK( self, ident, func, remove = False ):
    ident = ident.encode()
    cb    = self.getItem(ident).pyData
    ITF   = cb.setdefault( 'itf', CALLBACK_t() )
    if not func:
      func = ITF(0)
    elif type(type(func)) is not type(ITF):
      func = cb.setdefault( id(func), ITF(func) )
      remove and cb.pop( id(func) )
    return ident, func


  def connectCallback( self, ident, func ):
    ident, func = self._mk_CALLBACK( ident, func )
    return self.connect_callback_( byref(self), c_char_p(ident), func, c_int(len(ident)) )
  #
  # compatibility
  setCallback = connectCallback


  def disconnectCallback( self, ident, func = None ):
    ident, func = self._mk_CALLBACK( ident, func, remove=True )
    return self.disconnect_callback_( byref(self), c_char_p(ident), func, c_int(len(ident)) )


  def invokeCallback( self, ident, arg = None ):
    ident = ident.encode()
    argPtr = VOID_Ptr() if arg is None else byref(arg)
    self.invoke_callback_( byref(self), c_char_p(ident), argPtr, c_int(len(ident)) )


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
    if not hasattr( other, 'iteritems' ):
      other = dict( other )
    self._assign_tree( other )
    self._assign_tree( kwArgs )


  def updateDomain( self, path_dict, keyOp=str.split ):
    self._assign_tree( path_dict, keyOp )


  def iterDomain( self, paths, keyOp=str.split ):
    keyOp = keyOp or (lambda k: [k])
    try:
      for p in map( keyOp, paths ):
        yield p, self[p] if p else None
    except (TypeError, KeyError, AttributeError) as e:
      raise LookupError( "invalid path %s" % p )


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
      base = reduce( lambda d,k: d[k], ident[:-1], self )
      if type(type(base)) is type(Array): #< try on Scopes is expensive, so check for ctypes-Array!
        return base._type_.from_buffer( base, ident[-1] * sizeof(base._type_) )
      else:
        return base[ident[-1]]
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



ScopePtr = POINTER_t(Scope)
_mapType( 'HashMapPtr', 'type(HashMapPtr_t)', ScopePtr )

