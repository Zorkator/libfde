

######################################
class Stateful(object):
######################################
  """Mixin class extending ADTController types.
    
  Stateful provides cashed access to certain state scope, determined by option statePath

  """
  __opts__   = dict( statePath = '{rootId}' )

  @property
  def scopeKeySeparator( self ):
    return getattr( self._stock, '__keysep__', None )


  @scopeKeySeparator.setter
  def scopeKeySeparator( self, sep ):
    if   sep is None: op = None
    elif sep != ''  : op = lambda s: filter( bool, map( type(s).strip, s.split(sep) ) )
    else            : op = lambda s: s.split()
    self._stock.__keysep__ = op


  @property
  def state( self ):
    """return state scope, specified by option statePath."""
    try   : return self._stock._state
    except:
      self._stock._state = self._get_state()
      return self._stock._state


  def _get_state( self ):
    from adt.core import Scope
    path = self._statePath.format( **self.about ).split('/')
    return Scope.getProcessScope( *path )


  def setStateData( self, dataDict ):
    self.state.updateDomain( dataDict, self.scopeKeySeparator )
    return True


  def getStateData( self, keyList ):
    pairs = zip( *self.state.iterDomain( keyList, self.scopeKeySeparator ) )
    return pairs and pairs[1] or []

